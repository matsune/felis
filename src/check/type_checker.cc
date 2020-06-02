#include "check/type_checker.h"

#include "error/error.h"

namespace felis {

namespace {

bool TryResolve(std::shared_ptr<Type> ty, std::shared_ptr<Type> to) {
  std::cout << "TryResolve " << ty.get() << "(" << ToString(ty) << ") to "
            << to.get() << "(" << ToString(to) << ")" << std::endl;

  while (true) {
    if (to->IsFixed()) {
      break;
    } else if (to->IsUntyped()) {
      auto ref = std::dynamic_pointer_cast<Untyped>(to)->Ref();
      if (ref) {
        to = ref;
      } else {
        break;
      }
    } else {
      UNREACHABLE
    }
  }

  std::shared_ptr<Type> underlying = ty;
  while (true) {
    if (underlying->IsFixed()) {
      break;
    } else if (underlying->IsUntyped()) {
      auto ref = std::dynamic_pointer_cast<Untyped>(underlying)->Ref();
      if (ref) {
        if (ref == to) {
          return true;
        }
        underlying = ref;
      } else {
        break;
      }
    } else {
      UNREACHABLE
    }
  }

  if (underlying->IsFixed()) {
    if (underlying->IsArray()) {
      auto array_ty = std::dynamic_pointer_cast<ArrayType>(underlying);
      if (!to->IsArray()) return false;
      auto to_array_ty = std::dynamic_pointer_cast<ArrayType>(to);
      if (array_ty->size != to_array_ty->size) return false;
      return TryResolve(array_ty->elem, to_array_ty->elem);
    }
    return *underlying == *to;
  } else if (underlying->IsUntyped()) {
    auto untyped = std::dynamic_pointer_cast<Untyped>(underlying);
    if (untyped->Canbe(to)) {
      untyped->SetRef(to);
      return true;
    }
    return false;
  }
  UNREACHABLE
}

}  // namespace

void TypeChecker::Check(const std::unique_ptr<ast::File>& file) {
  // Insert function decls
  decl_ck_.CheckGlobalLevel(file);

  // Connects node and decl
  for (auto& fn : file->externs) {
    auto decl = decl_ck_.LookupFuncDecl(fn->proto->name->val);
    ctx_.RecordDecl(fn->proto->name, decl);
  }

  // Delve into each function blocks.
  // Check declaration, infer and check type at the same type.
  for (auto& fn : file->fn_decls) {
    auto decl = decl_ck_.LookupFuncDecl(fn->proto->name->val);
    ctx_.RecordDecl(fn->proto->name, decl);
    current_func_ = std::dynamic_pointer_cast<FuncType>(decl->type);
    std::cout << "Infer fn " << fn->proto->name->val << std::endl;

    decl_ck_.OpenScope();

    for (auto& arg : fn->proto->args->list) {
      // arg-name duplication is already checked in parser
      auto arg_decl = std::make_shared<Decl>(
          arg->name->val, decl_ck_.LookupType(arg->type_name), DeclKind::ARG);
      ctx_.RecordDecl(arg->name, arg_decl);
      decl_ck_.InsertDecl(arg->name->val, arg_decl);
    }

    auto result = CheckBlock(fn->block, false);
    if (result.IsNonValue()) {
      // function block ends with non-value statement
      // return type must be void
      if (!current_func_->ret->IsVoid()) {
        throw LocError::Create(fn->block->End(), "func type is not void");
      }
    } else if (result.IsExpr()) {
      // function block ends with expression
      // void function discards the value
      if (!current_func_->ret->IsVoid()) {
        // resolve type
        if (!TryResolve(result.val, current_func_->ret)) {
          throw LocError::Create(fn->block->End(), "mismatch ret type");
        }
      }
    }
  }
}

StmtResult<> TypeChecker::CheckBlock(const std::unique_ptr<ast::Block>& block,
                                     bool open_scope) {
  if (open_scope) decl_ck_.OpenScope();
  auto result = StmtResult<>::NonValue();
  for (auto i = 0; i < block->stmts.size(); ++i) {
    bool is_last = i == block->stmts.size() - 1;
    result = CheckStmt(block->stmts.at(i));
    if (!is_last && result.IsRet()) {
      throw LocError::Create(block->stmts.at(i + 1)->Begin(),
                             "unreachable code");
    }
  }
  if (open_scope) decl_ck_.CloseScope();
  return ctx_.RecordResult(block, result);
}

StmtResult<> TypeChecker::CheckStmt(const std::unique_ptr<ast::Stmt>& stmt) {
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      return CheckExpr((std::unique_ptr<ast::Expr>&)stmt);
    case ast::Stmt::Kind::RET:
      return CheckRet((std::unique_ptr<ast::RetStmt>&)stmt);
    case ast::Stmt::Kind::VAR_DECL:
      return CheckVarDecl((std::unique_ptr<ast::VarDeclStmt>&)stmt);
    case ast::Stmt::Kind::ASSIGN:
      return CheckAssign((std::unique_ptr<ast::AssignStmt>&)stmt);
  }
}

StmtResult<> TypeChecker::CheckRet(const std::unique_ptr<ast::RetStmt>& stmt) {
  bool is_void_fn = current_func_->ret->IsVoid();
  if (!stmt->expr) {
    if (!is_void_fn) {
      throw LocError::Create(stmt->End(), "ret is void");
    }
    return ctx_.RecordResult(stmt, StmtResult<>::Ret());
  }

  auto result = CheckExpr(stmt->expr);
  if (!is_void_fn) {
    if (result.IsNonValue()) {
      throw LocError::Create(stmt->expr->End(), "cannot return void type");
    } else if (result.IsExpr()) {
      if (!TryResolve(result.val, current_func_->ret)) {
        throw LocError::Create(stmt->expr->Begin(), "mismatch ret ty");
      }
    }
  }
  return ctx_.RecordResult(stmt, StmtResult<>::Ret());
}

StmtResult<> TypeChecker::CheckVarDecl(
    const std::unique_ptr<ast::VarDeclStmt>& stmt) {
  std::cout << "InferVarDecl" << std::endl;
  // name validation
  auto& name = stmt->name->val;
  if (decl_ck_.ExistsInThisScope(name)) {
    throw LocError::Create(stmt->Begin(), "redeclared var %s", name.c_str());
  }

  std::shared_ptr<Type> decl_ty = Unresolved();
  if (stmt->type_name) {
    // has type constraint
    decl_ty = decl_ck_.LookupType(stmt->type_name);
    if (!decl_ty) {
      throw LocError::Create(stmt->type_name->Begin(), "unknown decl type");
    }
  }

  auto stmt_ty = CheckExpr(stmt->expr);
  if (!stmt_ty.IsExpr()) {
    throw LocError::Create(stmt->Begin(), "cannot decl no type var");
  }
  auto expr_ty = stmt_ty.val;

  // resolve type constraints between decl and expr
  if (!TryResolve(decl_ty, expr_ty) && !TryResolve(expr_ty, decl_ty)) {
    throw LocError::Create(stmt->expr->Begin(), "mismatched decl type %s, %s",
                           ToString(decl_ty).c_str(),
                           ToString(expr_ty).c_str());
  }

  auto decl = std::make_shared<Decl>(
      name, decl_ty, stmt->is_let ? DeclKind::LET : DeclKind::VAR);
  decl_ck_.InsertDecl(name, decl);
  ctx_.RecordDecl(stmt->name, decl);
  return ctx_.RecordResult(stmt, StmtResult<>::NonValue());
}

StmtResult<> TypeChecker::CheckAssign(
    const std::unique_ptr<ast::AssignStmt>& stmt) {
  auto& name = stmt->name->val;
  auto decl = decl_ck_.LookupVarDecl(name);
  if (!decl) {
    throw LocError::Create(stmt->Begin(), "undeclared var %s", name.c_str());
  }
  if (!decl->IsAssignable()) {
    throw LocError::Create(stmt->Begin(), "%s is declared as mutable variable",
                           name.c_str());
  }
  ctx_.RecordDecl(stmt->name, decl);

  auto stmt_ty = CheckExpr(stmt->expr);
  if (!stmt_ty.IsExpr()) {
    throw LocError::Create(stmt->Begin(), "cannot assign no type var");
  }
  auto expr_ty = stmt_ty.val;

  if (!TryResolve(decl->type, expr_ty) && !TryResolve(expr_ty, decl->type)) {
    throw LocError::Create(
        stmt->expr->Begin(), "mismatched assign type %s = %s",
        ToString(decl->type).c_str(), ToString(expr_ty).c_str());
  }
  return ctx_.RecordResult(stmt, StmtResult<>::NonValue());
}

StmtResult<> TypeChecker::CheckExpr(const std::unique_ptr<ast::Expr>& expr) {
  std::cout << "InferExpr " << ToString(expr->ExprKind()) << std::endl;
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT:
      return CheckLit((std::unique_ptr<ast::Lit>&)expr);
    case ast::Expr::Kind::IDENT:
      return CheckIdent((std::unique_ptr<ast::Ident>&)expr);
    case ast::Expr::Kind::BINARY:
      return CheckBinary((std::unique_ptr<ast::BinaryExpr>&)expr);
    case ast::Expr::Kind::UNARY:
      return CheckUnary((std::unique_ptr<ast::UnaryExpr>&)expr);
    case ast::Expr::Kind::CALL:
      return CheckCall((std::unique_ptr<ast::CallExpr>&)expr);
    case ast::Expr::Kind::ARRAY:
      return CheckArray((std::unique_ptr<ast::ArrayExpr>&)expr);
    case ast::Expr::Kind::IF:
      return CheckIf((std::unique_ptr<ast::If>&)expr);
    case ast::Expr::Kind::BLOCK:
      return CheckBlock((std::unique_ptr<ast::Block>&)expr);
  }
}

StmtResult<> TypeChecker::CheckLit(const std::unique_ptr<ast::Lit>& lit) {
  std::shared_ptr<Type> ty;
  switch (lit->LitKind()) {
    case ast::Lit::Kind::BOOL:
      ty = kTypeBool;
      break;
    case ast::Lit::Kind::STRING:
      ty = kTypeString;
      break;
    case ast::Lit::Kind::CHAR:
    case ast::Lit::Kind::INT:
      ty = UntypedInt();
      break;
    case ast::Lit::Kind::FLOAT:
      ty = UntypedFloat();
      break;
  }
  return ctx_.RecordResult(lit, StmtResult<>::Expr(ty));
}

StmtResult<> TypeChecker::CheckIdent(const std::unique_ptr<ast::Ident>& ident) {
  auto decl = decl_ck_.LookupVarDecl(ident->val);
  if (!decl) {
    throw LocError::Create(ident->Begin(), "undefined function %s",
                           ident->val.c_str());
  }
  ctx_.RecordDecl(ident, decl);
  return ctx_.RecordResult(ident, StmtResult<>::Expr(decl->type));
}

StmtResult<> TypeChecker::CheckBinary(
    const std::unique_ptr<ast::BinaryExpr>& binary) {
  auto lhs_stmt_ty = CheckExpr(binary->lhs);
  if (!lhs_stmt_ty.IsExpr()) {
    throw LocError::Create(binary->lhs->Begin(), "non type lhs ty");
  }
  auto lhs_ty = lhs_stmt_ty.val;
  auto rhs_stmt_ty = CheckExpr(binary->rhs);
  if (!rhs_stmt_ty.IsExpr()) {
    throw LocError::Create(binary->rhs->Begin(), "non type rhs ty");
  }
  auto rhs_ty = rhs_stmt_ty.val;

  std::shared_ptr<Type> operand_ty;
  if (TryResolve(lhs_ty, rhs_ty)) {
    operand_ty = rhs_ty;
  } else if (TryResolve(rhs_ty, lhs_ty)) {
    operand_ty = lhs_ty;
  } else {
    throw LocError::Create(binary->lhs->Begin(), "unmatch type");
  }

  std::shared_ptr<Type> ty;
  switch (binary->op->op) {
    case ast::BinaryOp::Op::EQEQ:
    case ast::BinaryOp::Op::NEQ:
    case ast::BinaryOp::Op::LT:
    case ast::BinaryOp::Op::LE:
    case ast::BinaryOp::Op::GT:
    case ast::BinaryOp::Op::GE:
      if (operand_ty->IsString() || operand_ty->IsVoid()) {
        throw LocError::Create(binary->Begin(),
                               "cannot use this type for comparison");
      }
      ty = kTypeBool;
      break;

    case ast::BinaryOp::Op::ADD:
    case ast::BinaryOp::Op::SUB:
    case ast::BinaryOp::Op::MUL:
    case ast::BinaryOp::Op::DIV:
      if (!operand_ty->IsUntyped() && !operand_ty->IsFixedNumeric()) {
        throw LocError::Create(binary->Begin(),
                               "cannot use non numeric type of binary");
      }
      ty = operand_ty;
      break;

    case ast::BinaryOp::Op::MOD:
      if (!operand_ty->IsUntyped() && !operand_ty->IsFixedInt()) {
        throw LocError::Create(binary->Begin(),
                               "cannot use non numeric type of binary");
      }
      ty = operand_ty;
      break;
  }
  return ctx_.RecordResult(binary, StmtResult<>::Expr(ty));
}

StmtResult<> TypeChecker::CheckUnary(
    const std::unique_ptr<ast::UnaryExpr>& unary) {
  auto stmt_ty = CheckExpr(unary->expr);
  if (!stmt_ty.IsExpr()) {
    throw LocError::Create(unary->Begin(), "non type unary ty");
  }
  return ctx_.RecordResult(unary, StmtResult<>::Expr(stmt_ty.val));
}

StmtResult<> TypeChecker::CheckCall(
    const std::unique_ptr<ast::CallExpr>& call) {
  auto decl = decl_ck_.LookupFuncDecl(call->ident->val);
  if (decl == nullptr) {
    throw LocError::Create(call->Begin(), "undefined function %s",
                           call->ident->val.c_str());
  }
  auto fn_type = decl->AsFuncType();
  if (fn_type->args.size() != call->args.size()) {
    throw LocError::Create(call->Begin(), "args count doesn't match");
  }

  for (auto i = 0; i < call->args.size(); ++i) {
    auto& arg = call->args.at(i);
    auto stmt_ty = CheckExpr(arg);
    if (!stmt_ty.IsExpr()) {
      throw LocError::Create(arg->Begin(), "non type arg ty");
    }
    auto arg_ty = stmt_ty.val;
    if (!TryResolve(arg_ty, fn_type->args[i])) {
      throw LocError::Create(arg->Begin(), "mismatched arg ty");
    }
  }
  ctx_.RecordDecl(call->ident, decl);
  return ctx_.RecordResult(call, StmtResult<>::Expr(fn_type->ret));
}

StmtResult<> TypeChecker::CheckArray(
    const std::unique_ptr<ast::ArrayExpr>& array) {
  auto size = array->exprs.size();
  auto elem_ty = Unresolved();
  for (auto& expr : array->exprs) {
    auto stmt_ty = CheckExpr(expr);
    if (!stmt_ty.IsExpr()) {
      throw LocError::Create(expr->Begin(), "array expr not type");
    }
    if (!TryResolve(elem_ty, stmt_ty.val)) {
      throw LocError::Create(expr->Begin(), "mismatch element type");
    }
  }
  return ctx_.RecordResult(
      array, StmtResult<>::Expr(std::make_shared<ArrayType>(elem_ty, size)));
}

StmtResult<> TypeChecker::CheckIf(const std::unique_ptr<ast::If>& e) {
  auto cond_stmt_ty = CheckExpr(e->cond);
  if (!cond_stmt_ty.IsExpr()) {
    throw LocError::Create(e->cond->Begin(), "cond is not type");
  }
  auto cond_ty = cond_stmt_ty.val;
  if (!TryResolve(cond_ty, kTypeBool)) {
    throw LocError::Create(e->cond->Begin(),
                           "if-statement condition must be bool type");
  }

  auto block_stmt_ty = CheckBlock(e->block);

  if (!e->HasElse()) {
    // Incomp
    return ctx_.RecordResult(e, StmtResult<>::NonValue());
  }

  // comp
  auto els_stmt_ty = StmtResult<>::NonValue();
  if (e->IsElseIf()) {
    els_stmt_ty = CheckIf((std::unique_ptr<ast::If>&)e->els);
  } else {
    els_stmt_ty = CheckBlock((std::unique_ptr<ast::Block>&)e->els);
  }

  // NON > EXPR > RET

  if (block_stmt_ty.IsNonValue() || els_stmt_ty.IsNonValue()) {
    return ctx_.RecordResult(e, StmtResult<>::NonValue());
  }

  if (block_stmt_ty.IsRet() && els_stmt_ty.IsRet()) {
    return ctx_.RecordResult(e, StmtResult<>::Ret());
  }

  std::shared_ptr<Type> whole_ty = kTypeVoid;
  if (block_stmt_ty.IsRet()) {
    whole_ty = els_stmt_ty.val;
  } else if (els_stmt_ty.IsRet()) {
    whole_ty = block_stmt_ty.val;
  } else {
    // expr and expr
    // resolve type
    auto block_ty = block_stmt_ty.val;
    auto els_ty = els_stmt_ty.val;
    if (TryResolve(block_ty, els_ty)) {
      whole_ty = els_ty;
    } else if (TryResolve(els_ty, block_ty)) {
      whole_ty = block_ty;
    } else {
      throw LocError::Create(e->Begin(), "unmatched els branches types");
    }
  }
  return ctx_.RecordResult(e, StmtResult<>::Expr(whole_ty));
}

}  // namespace felis

