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
  decl_ck_.CheckGlobalLevel(file);

  for (auto& fn : file->externs) {
    auto decl = decl_ck_.LookupFuncDecl(fn->proto->name->val);
    ctx_.RecordDecl(fn->proto->name, decl);
  }

  for (auto& fn : file->fn_decls) {
    auto decl = decl_ck_.LookupFuncDecl(fn->proto->name->val);
    ctx_.RecordDecl(fn->proto->name, decl);
    current_func_ = decl->AsFuncType();
    std::cout << "Infer fn " << fn->proto->name->val << std::endl;

    decl_ck_.OpenScope();

    for (auto& arg : fn->proto->args->list) {
      // arg-name duplication is already checked in parser
      auto arg_decl = std::make_shared<Decl>(
          arg->name->val, decl_ck_.LookupType(arg->type_name), DeclKind::ARG);
      ctx_.RecordDecl(arg->name, arg_decl);

      decl_ck_.InsertDecl(arg->name->val, arg_decl);
    }

    auto block_ty = InferBlock(fn->block);
    if (!fn->block->IsTerminating() && !current_func_->ret->IsVoid()) {
      std::cout << "fn->block " << fn->block.get() << " is not terminating"
                << std::endl;
      if (!TryResolve(block_ty, current_func_->ret)) {
        throw LocError::Create(fn->block->End(), "mismatch ret type");
      }
    }

    decl_ck_.CloseScope();
  }
}

std::shared_ptr<Type> TypeChecker::InferStmt(
    const std::unique_ptr<ast::Stmt>& stmt) {
  std::cout << "InferStmt " << ToString(stmt->StmtKind()) << std::endl;

  if (stmt->parent->IsBlockRet() && stmt->IsLastStmt()) {
    switch (stmt->StmtKind()) {
      case ast::Stmt::Kind::VAR_DECL:
      case ast::Stmt::Kind::ASSIGN:
        throw LocError::Create(stmt->Begin(), "not value return statement");
      default:
        break;
    }
  }

  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      return InferExpr((std::unique_ptr<ast::Expr>&)stmt);
    case ast::Stmt::Kind::RET:
      InferRet((std::unique_ptr<ast::RetStmt>&)stmt);
      break;
    case ast::Stmt::Kind::VAR_DECL:
      InferVarDecl((std::unique_ptr<ast::VarDeclStmt>&)stmt);
      break;
    case ast::Stmt::Kind::ASSIGN:
      InferAssign((std::unique_ptr<ast::AssignStmt>&)stmt);
      break;
  }
  return kTypeVoid;
}

void TypeChecker::InferRet(const std::unique_ptr<ast::RetStmt>& stmt) {
  bool is_void_func = current_func_->ret->IsVoid();
  bool is_ret_void = stmt->expr == nullptr;

  if (is_void_func != is_ret_void) {
    if (is_void_func) {
      throw LocError::Create(stmt->expr->Begin(), "func type is void");
    } else {
      throw LocError::Create(stmt->Begin(), "func type is not void");
    }
  }

  if (stmt->expr) {
    auto expr_ty = InferExpr(stmt->expr);
    if (!TryResolve(expr_ty, current_func_->ret)) {
      throw LocError::Create(stmt->Begin(), "mismatch ret type");
    }
  }
  std::cout << "end ret" << std::endl;
}

void TypeChecker::InferVarDecl(const std::unique_ptr<ast::VarDeclStmt>& stmt) {
  std::cout << "InferVarDecl" << std::endl;
  // name validation
  auto& name = stmt->name->val;
  if (!decl_ck_.CanDecl(name)) {
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

  auto expr_ty = InferExpr(stmt->expr);
  if (expr_ty->IsVoid()) {
    throw LocError::Create(stmt->Begin(), "cannot decl void type var");
  }

  // resolve type constraints between decl and expr
  if (!TryResolve(decl_ty, expr_ty) && !TryResolve(expr_ty, decl_ty)) {
    throw LocError::Create(stmt->expr->Begin(), "mismatched decl type %s, %s",
                           ToString(decl_ty).c_str(),
                           ToString(expr_ty).c_str());
  }

  auto decl = std::make_shared<Decl>(
      name, decl_ty, stmt->is_let ? DeclKind::LET : DeclKind::VAR);
  ctx_.RecordDecl(stmt->name, decl);

  decl_ck_.InsertDecl(name, decl);
}

void TypeChecker::InferAssign(const std::unique_ptr<ast::AssignStmt>& stmt) {
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

  auto expr_ty = InferExpr(stmt->expr);
  if (!TryResolve(decl->type, expr_ty) && !TryResolve(expr_ty, decl->type)) {
    throw LocError::Create(
        stmt->expr->Begin(), "mismatched assign type %s = %s",
        ToString(decl->type).c_str(), ToString(expr_ty).c_str());
  }
}

std::shared_ptr<Type> TypeChecker::InferExpr(
    const std::unique_ptr<ast::Expr>& expr) {
  std::cout << "InferExpr " << ToString(expr->ExprKind()) << std::endl;
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT: {
      auto& lit = (std::unique_ptr<ast::Lit>&)expr;
      switch (lit->LitKind()) {
        case ast::Lit::Kind::BOOL:
          return ctx_.RecordType(lit, kTypeBool);
        case ast::Lit::Kind::STRING:
          return ctx_.RecordType(lit, kTypeString);
        case ast::Lit::Kind::CHAR:
        case ast::Lit::Kind::INT:
          return ctx_.RecordType(lit, UntypedInt());
        case ast::Lit::Kind::FLOAT:
          return ctx_.RecordType(lit, UntypedFloat());
      }
    } break;

    case ast::Expr::Kind::CALL: {
      auto& call_expr = (std::unique_ptr<ast::CallExpr>&)expr;
      auto decl = decl_ck_.LookupFuncDecl(call_expr->ident->val);
      if (decl == nullptr) {
        throw LocError::Create(call_expr->Begin(), "undefined function %s",
                               call_expr->ident->val.c_str());
      }
      auto fn_type = decl->AsFuncType();
      if (fn_type->args.size() != call_expr->args.size()) {
        throw LocError::Create(call_expr->Begin(), "args count doesn't match");
      }

      for (auto i = 0; i < call_expr->args.size(); ++i) {
        auto& arg = call_expr->args.at(i);
        auto arg_ty = InferExpr(arg);
        if (!TryResolve(arg_ty, fn_type->args[i])) {
          throw LocError::Create(arg->Begin(), "mismatched arg ty");
        }
      }
      ctx_.RecordDecl(call_expr->ident, decl);
      return ctx_.RecordType(call_expr, fn_type->ret);
    } break;

    case ast::Expr::Kind::IDENT: {
      auto& ident = (std::unique_ptr<ast::Ident>&)expr;
      auto decl = decl_ck_.LookupVarDecl(ident->val);
      if (!decl) {
        throw LocError::Create(ident->Begin(), "undefined function %s",
                               ident->val.c_str());
      }
      ctx_.RecordDecl(ident, decl);
      return ctx_.RecordType(ident, decl->type);
    } break;

    case ast::Expr::Kind::UNARY: {
      auto& unary_expr = (std::unique_ptr<ast::UnaryExpr>&)expr;
      return ctx_.RecordType(unary_expr, InferExpr(unary_expr->expr));
    } break;

    case ast::Expr::Kind::BINARY: {
      auto& binary = (std::unique_ptr<ast::BinaryExpr>&)expr;
      auto lhs_ty = InferExpr(binary->lhs);
      auto rhs_ty = InferExpr(binary->rhs);

      std::shared_ptr<Type> operand_ty;
      if (TryResolve(lhs_ty, rhs_ty)) {
        operand_ty = rhs_ty;
      } else if (TryResolve(rhs_ty, lhs_ty)) {
        operand_ty = lhs_ty;
      } else {
        throw LocError::Create(binary->lhs->Begin(), "unmatch type");
      }

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
          return ctx_.RecordType(binary, kTypeBool);

        case ast::BinaryOp::Op::ADD:
        case ast::BinaryOp::Op::SUB:
        case ast::BinaryOp::Op::MUL:
        case ast::BinaryOp::Op::DIV:
          if (!operand_ty->IsUntyped() && !operand_ty->IsFixedNumeric()) {
            throw LocError::Create(binary->Begin(),
                                   "cannot use non numeric type of binary");
          }
          return ctx_.RecordType(binary, operand_ty);

        case ast::BinaryOp::Op::MOD:
          if (!operand_ty->IsUntyped() && !operand_ty->IsFixedInt()) {
            throw LocError::Create(binary->Begin(),
                                   "cannot use non numeric type of binary");
          }
          return ctx_.RecordType(binary, operand_ty);
      }
    } break;

    case ast::Expr::Kind::IF:
      return InferIf((std::unique_ptr<ast::If>&)expr);
    case ast::Expr::Kind::BLOCK:
      return InferBlock((std::unique_ptr<ast::Block>&)expr);

    case ast::Expr::Kind::ARRAY: {
      auto& array = (std::unique_ptr<ast::ArrayExpr>&)expr;
      auto size = array->exprs.size();
      auto elem_ty = Unresolved();
      for (auto& e : array->exprs) {
        auto e_ty = InferExpr(e);
        if (!TryResolve(elem_ty, e_ty)) {
          throw LocError::Create(e->Begin(), "mismatch element type");
        }
      }
      return ctx_.RecordType(array, std::make_shared<ArrayType>(elem_ty, size));
    } break;
  }
}

std::shared_ptr<Type> TypeChecker::InferIf(const std::unique_ptr<ast::If>& e) {
  auto cond_ty = InferExpr(e->cond);
  if (!TryResolve(cond_ty, kTypeBool)) {
    throw LocError::Create(e->cond->Begin(),
                           "if-statement condition must be bool type");
  }

  auto block_ty = InferBlock(e->block);

  if (!e->HasElse()) {
    // no else
    if (!e->as_stmt) {
      // No else if-statement can't be used as value
      //
      // ex) let a = if true { 4 }
      //
      throw LocError::Create(e->Begin(), "incomplete if statement");
    }
    return ctx_.RecordType(e, kTypeVoid);
  }

  // has else

  std::shared_ptr<Type> whole_ty = kTypeVoid;
  auto is_then_terminating = e->block->IsTerminating();
  auto is_else_terminating = e->els->IsTerminating();

  std::shared_ptr<Type> els_ty;
  if (e->IsElseIf()) {
    els_ty = InferIf((std::unique_ptr<ast::If>&)e->els);
  } else {
    els_ty = InferBlock((std::unique_ptr<ast::Block>&)e->els);
  }

  if (e->IsBlockRet() || !e->as_stmt) {
    // requires complete if-stmt
    if (is_then_terminating && is_else_terminating) {
      // Cannot infer type like this
      //
      // ```
      // let a = if true {
      //    ret 3
      //  } else {
      //    ret 4
      //  }
      // ```
      //
      throw LocError::Create(e->Begin(), "returning in all branch");
    }

    if (is_then_terminating) {
      whole_ty = els_ty;
    } else if (is_else_terminating) {
      whole_ty = block_ty;
    } else {
      // Both of then block and else block should be same type
      if (TryResolve(block_ty, els_ty)) {
        whole_ty = els_ty;
      } else if (TryResolve(els_ty, block_ty)) {
        whole_ty = block_ty;
      } else {
        throw LocError::Create(e->Begin(), "unmatched if branches types");
      }
    }
  }

  return ctx_.RecordType(e, whole_ty);
}

std::shared_ptr<Type> TypeChecker::InferBlock(
    const std::unique_ptr<ast::Block>& e) {
  std::cout << "InferBlock" << std::endl;
  decl_ck_.OpenScope();
  std::shared_ptr<Type> ty = kTypeVoid;
  for (auto& stmt : e->stmts) {
    ty = InferStmt(stmt);
  }
  decl_ck_.CloseScope();
  return ctx_.RecordType(e, ty);
}

}  // namespace felis

