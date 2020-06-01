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

    auto ty = CheckBlock(fn->block);
    if (ty.IsNon()) {
      // ret must void
      if (!current_func_->ret->IsVoid()) {
        throw LocError::Create(fn->block->End(), "func type is not void");
      }
    } else if (ty.IsType() && !current_func_->ret->IsVoid()) {
      // resolve
      if (!TryResolve(ty.ty, current_func_->ret)) {
        throw LocError::Create(fn->block->End(), "mismatch ret type");
      }
    }

    decl_ck_.CloseScope();
  }
}

StmtType TypeChecker::CheckStmt(const std::unique_ptr<ast::Stmt>& stmt) {
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      return CheckExpr((std::unique_ptr<ast::Expr>&)stmt);

    case ast::Stmt::Kind::RET: {
      auto& ret = (std::unique_ptr<ast::RetStmt>&)stmt;
      if (ret->expr) {
        auto expr_ty = CheckExpr(ret->expr);
        if (expr_ty.IsRet()) {
          return StmtType(StmtType::Kind::RET);
        } else if (expr_ty.IsNon()) {
          throw LocError::Create(ret->expr->Begin(), "non type");
        } else if (expr_ty.IsType()) {
          if (!TryResolve(expr_ty.ty, current_func_->ret)) {
            throw LocError::Create(ret->expr->Begin(), "mismatch ret ty");
          }
        }
      } else {
        if (!current_func_->ret->IsVoid()) {
          throw LocError::Create(ret->End(), "ret is void");
        }
      }
      return StmtType(StmtType::Kind::RET);
    } break;

    case ast::Stmt::Kind::VAR_DECL:
      CheckVarDecl((std::unique_ptr<ast::VarDeclStmt>&)stmt);
      return StmtType(StmtType::Kind::NON);

    case ast::Stmt::Kind::ASSIGN:
      CheckAssign((std::unique_ptr<ast::AssignStmt>&)stmt);
      return StmtType(StmtType::Kind::NON);
  }
}

StmtType TypeChecker::CheckBlock(const std::unique_ptr<ast::Block>& block) {
  decl_ck_.OpenScope();
  StmtType stmt_ty(StmtType::Kind::NON);
  for (auto& stmt : block->stmts) {
    stmt_ty = CheckStmt(stmt);
  }
  decl_ck_.CloseScope();
  if (stmt_ty.IsType()) {
    ctx_.RecordType(block, stmt_ty.ty);
  }
  return stmt_ty;
  /* return ctx_.RecordType(block, ty); */
}

void TypeChecker::CheckVarDecl(const std::unique_ptr<ast::VarDeclStmt>& stmt) {
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

  auto stmt_ty = CheckExpr(stmt->expr);
  if (!stmt_ty.IsType()) {
    throw LocError::Create(stmt->Begin(), "cannot decl no type var");
  }

  auto expr_ty = stmt_ty.ty;

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

void TypeChecker::CheckAssign(const std::unique_ptr<ast::AssignStmt>& stmt) {
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
  if (!stmt_ty.IsType()) {
    throw LocError::Create(stmt->Begin(), "cannot assign no type var");
  }
  auto expr_ty = stmt_ty.ty;

  if (!TryResolve(decl->type, expr_ty) && !TryResolve(expr_ty, decl->type)) {
    throw LocError::Create(
        stmt->expr->Begin(), "mismatched assign type %s = %s",
        ToString(decl->type).c_str(), ToString(expr_ty).c_str());
  }
}

StmtType TypeChecker::CheckExpr(const std::unique_ptr<ast::Expr>& expr) {
  std::cout << "InferExpr " << ToString(expr->ExprKind()) << std::endl;
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT: {
      auto& lit = (std::unique_ptr<ast::Lit>&)expr;
      std::shared_ptr<Type> ty;
      switch (lit->LitKind()) {
        case ast::Lit::Kind::BOOL:
          ty = ctx_.RecordType(lit, kTypeBool);
          break;
        case ast::Lit::Kind::STRING:
          ty = ctx_.RecordType(lit, kTypeString);
          break;
        case ast::Lit::Kind::CHAR:
        case ast::Lit::Kind::INT:
          ty = ctx_.RecordType(lit, UntypedInt());
          break;
        case ast::Lit::Kind::FLOAT:
          ty = ctx_.RecordType(lit, UntypedFloat());
          break;
      }
      return StmtType(StmtType::Kind::TYPE, ty);
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
        auto stmt_ty = CheckExpr(arg);
        if (!stmt_ty.IsType()) {
          throw LocError::Create(arg->Begin(), "non type arg ty");
        }
        auto arg_ty = stmt_ty.ty;
        if (!TryResolve(arg_ty, fn_type->args[i])) {
          throw LocError::Create(arg->Begin(), "mismatched arg ty");
        }
      }
      ctx_.RecordDecl(call_expr->ident, decl);
      auto ty = ctx_.RecordType(call_expr, fn_type->ret);
      return StmtType(StmtType::Kind::TYPE, ty);
    } break;

    case ast::Expr::Kind::IDENT: {
      auto& ident = (std::unique_ptr<ast::Ident>&)expr;
      auto decl = decl_ck_.LookupVarDecl(ident->val);
      if (!decl) {
        throw LocError::Create(ident->Begin(), "undefined function %s",
                               ident->val.c_str());
      }
      ctx_.RecordDecl(ident, decl);
      auto ty = ctx_.RecordType(ident, decl->type);
      return StmtType(StmtType::Kind::TYPE, ty);
    } break;

    case ast::Expr::Kind::UNARY: {
      auto& unary_expr = (std::unique_ptr<ast::UnaryExpr>&)expr;
      auto stmt_ty = CheckExpr(unary_expr->expr);
      if (!stmt_ty.IsType()) {
        throw LocError::Create(unary_expr->Begin(), "non type unary ty");
      }
      auto ty = ctx_.RecordType(unary_expr, stmt_ty.ty);
      return StmtType(StmtType::Kind::TYPE, ty);
    } break;

    case ast::Expr::Kind::BINARY: {
      auto& binary = (std::unique_ptr<ast::BinaryExpr>&)expr;
      auto lhs_stmt_ty = CheckExpr(binary->lhs);
      if (!lhs_stmt_ty.IsType()) {
        throw LocError::Create(binary->lhs->Begin(), "non type lhs ty");
      }
      auto lhs_ty = lhs_stmt_ty.ty;
      auto rhs_stmt_ty = CheckExpr(binary->rhs);
      if (!rhs_stmt_ty.IsType()) {
        throw LocError::Create(binary->rhs->Begin(), "non type rhs ty");
      }
      auto rhs_ty = rhs_stmt_ty.ty;

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
          ty = ctx_.RecordType(binary, kTypeBool);
          break;

        case ast::BinaryOp::Op::ADD:
        case ast::BinaryOp::Op::SUB:
        case ast::BinaryOp::Op::MUL:
        case ast::BinaryOp::Op::DIV:
          if (!operand_ty->IsUntyped() && !operand_ty->IsFixedNumeric()) {
            throw LocError::Create(binary->Begin(),
                                   "cannot use non numeric type of binary");
          }
          ty = ctx_.RecordType(binary, operand_ty);
          break;

        case ast::BinaryOp::Op::MOD:
          if (!operand_ty->IsUntyped() && !operand_ty->IsFixedInt()) {
            throw LocError::Create(binary->Begin(),
                                   "cannot use non numeric type of binary");
          }
          ty = ctx_.RecordType(binary, operand_ty);
          break;
      }
      return StmtType(StmtType::Kind::TYPE, ty);
    } break;

    case ast::Expr::Kind::IF:
      return CheckIf((std::unique_ptr<ast::If>&)expr);
    case ast::Expr::Kind::BLOCK:
      return CheckBlock((std::unique_ptr<ast::Block>&)expr);

    case ast::Expr::Kind::ARRAY: {
      auto& array = (std::unique_ptr<ast::ArrayExpr>&)expr;
      auto size = array->exprs.size();
      auto elem_ty = Unresolved();
      for (auto& expr : array->exprs) {
        auto stmt_ty = CheckExpr(expr);
        if (!stmt_ty.IsType()) {
          throw LocError::Create(expr->Begin(), "array expr not type");
        }
        if (!TryResolve(elem_ty, stmt_ty.ty)) {
          throw LocError::Create(expr->Begin(), "mismatch element type");
        }
      }
      auto ty =
          ctx_.RecordType(array, std::make_shared<ArrayType>(elem_ty, size));
      return StmtType(StmtType::Kind::TYPE, ty);
    } break;
  }
}

StmtType TypeChecker::CheckIf(const std::unique_ptr<ast::If>& e) {
  auto cond_stmt_ty = CheckExpr(e->cond);
  if (!cond_stmt_ty.IsType()) {
    throw LocError::Create(e->cond->Begin(), "cond is not type");
  }
  auto cond_ty = cond_stmt_ty.ty;
  if (!TryResolve(cond_ty, kTypeBool)) {
    throw LocError::Create(e->cond->Begin(),
                           "if-statement condition must be bool type");
  }

  auto block_stmt_ty = CheckBlock(e->block);

  if (!e->HasElse()) {
    ctx_.RecordType(e, kTypeVoid);
    // Incomp
    return StmtType(StmtType::Kind::NON);
  }

  // comp

  StmtType els_stmt_ty(StmtType::NON);
  if (e->IsElseIf()) {
    els_stmt_ty = CheckIf((std::unique_ptr<ast::If>&)e->els);
  } else {
    els_stmt_ty = CheckBlock((std::unique_ptr<ast::Block>&)e->els);
  }
  auto then_terminating = block_stmt_ty.IsRet();
  auto els_terminating = els_stmt_ty.IsRet();

  if (then_terminating && els_terminating) {
    // all ret
    ctx_.RecordType(e, kTypeVoid);
    return StmtType(StmtType::Kind::RET);
  }

  // partial ret

  if (e->IsResultDiscard()) {
    // no need to resolve type
    ctx_.RecordType(e, kTypeVoid);
    return StmtType(StmtType::NON);
  }

  // resolve type
  if (block_stmt_ty.IsNon()) {
    throw LocError::Create(e->block->Begin(), "then block not type");
  }
  if (els_stmt_ty.IsNon()) {
    throw LocError::Create(e->els->Begin(), "els block not type");
  }

  std::shared_ptr<Type> whole_ty = kTypeVoid;
  if (then_terminating) {
    assert(els_stmt_ty.IsType());
    whole_ty = els_stmt_ty.ty;
  } else if (els_terminating) {
    assert(block_stmt_ty.IsType());
    whole_ty = block_stmt_ty.ty;
  } else {
    assert(block_stmt_ty.IsType());
    assert(els_stmt_ty.IsType());
    auto block_ty = block_stmt_ty.ty;
    auto els_ty = els_stmt_ty.ty;
    if (TryResolve(block_ty, els_ty)) {
      whole_ty = els_ty;
    } else if (TryResolve(els_ty, block_ty)) {
      whole_ty = block_ty;
    } else {
      throw LocError::Create(e->Begin(), "unmatched els branches types");
    }
  }
  auto ty = ctx_.RecordType(e, whole_ty);
  return StmtType(StmtType::Kind::TYPE, ty);
}

}  // namespace felis

