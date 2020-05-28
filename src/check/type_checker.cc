#include "check/type_checker.h"

#include "error/error.h"

namespace felis {

namespace {

bool TryResolve(std::shared_ptr<Type> ty, std::shared_ptr<Type> to) {
  std::cout << "TryResolve " << ty.get() << "(" << ToString(ty) << ") to "
            << to.get() << "(" << ToString(to) << ")" << std::endl;
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
  decl_ck.CheckGlobalLevel(file);

  for (auto& fn : file->externs) {
    auto decl = decl_ck.LookupFuncDecl(fn->proto->name->val);
    RecordDecl(fn->proto->name, decl);
  }

  for (auto& fn : file->fn_decls) {
    auto decl = decl_ck.LookupFuncDecl(fn->proto->name->val);
    RecordDecl(fn->proto->name, decl);
    current_func_ = decl->AsFuncType();
    std::cout << "Infer fn " << fn->proto->name->val << std::endl;

    decl_ck.OpenScope();

    for (auto& arg : fn->proto->args->list) {
      // arg-name duplication is already checked in parser
      auto arg_decl = std::make_shared<Decl>(
          arg->name->val, decl_ck.LookupType(arg->type_name), DeclKind::ARG);
      RecordDecl(arg->name, arg_decl);

      decl_ck.InsertDecl(arg->name->val, arg_decl);
    }

    std::shared_ptr<Type> block_ty = kTypeVoid;
    for (auto it = fn->block->stmts.begin(); it != fn->block->stmts.end();
         ++it) {
      bool is_end = std::next(it) == fn->block->stmts.end();
      const std::unique_ptr<ast::Stmt>& stmt = *it;
      block_ty = InferStmt(stmt, false);

      if (!is_end && stmt->IsTerminating()) {
        auto next = std::next(it);
        throw LocError::Create((*next)->Begin(), "unreachable code");
      }
    }
    if (!current_func_->ret->IsVoid() && !fn->block->IsTerminating()) {
      throw LocError::Create(fn->block->End(), "no return");
    }
    RecordType(fn->block, block_ty);

    decl_ck.CloseScope();
  }
}

//
// Infer a type of `ast::Stmt`.
// `as_expr` means whether the statement would be used as a value
// for other statement. For example, block is a kind of expressions
// but it consists of a list of statements, and the last statement
// may be a value of the block.
//
// ```
// hoge()
//
// let a = {
//    let b = 2
//    fuga(b)
// }
// ```
// In this case, `hoge()` is just a statement because result value
// won't be used. However, `fuga(b)` is a statement but also a value
// of block, this means the result value will be assigned into
// variable `a`.
//
std::shared_ptr<Type> TypeChecker::InferStmt(
    const std::unique_ptr<ast::Stmt>& stmt, bool as_expr) {
  std::cout << "InferStmt " << ToString(stmt->StmtKind()) << std::endl;

  if (as_expr) {
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
      return InferExpr((std::unique_ptr<ast::Expr>&)stmt, as_expr);
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
    auto expr_ty = InferExpr(stmt->expr, true);
    if (!TryResolve(expr_ty, current_func_->ret)) {
      throw LocError::Create(stmt->Begin(), "mismatch ret type");
    }
  }
  std::cout << "end ret" << std::endl;
}

void TypeChecker::InferVarDecl(const std::unique_ptr<ast::VarDeclStmt>& stmt) {
  // name validation
  auto& name = stmt->name->val;
  if (!decl_ck.CanDecl(name)) {
    throw LocError::Create(stmt->Begin(), "redeclared var %s", name.c_str());
  }
  std::shared_ptr<Type> decl_ty = Unresolved();
  if (stmt->type_name) {
    // has type constraint
    decl_ty = decl_ck.LookupType(stmt->type_name);
    if (!decl_ty) {
      throw LocError::Create(stmt->type_name->Begin(), "unknown decl type");
    }
  }

  auto expr_ty = InferExpr(stmt->expr, true);
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
  RecordDecl(stmt->name, decl);

  decl_ck.InsertDecl(name, decl);
}

void TypeChecker::InferAssign(const std::unique_ptr<ast::AssignStmt>& stmt) {
  auto& name = stmt->name->val;
  auto decl = decl_ck.LookupVarDecl(name);
  if (!decl) {
    throw LocError::Create(stmt->Begin(), "undeclared var %s", name.c_str());
  }
  if (!decl->IsAssignable()) {
    throw LocError::Create(stmt->Begin(), "%s is declared as mutable variable",
                           name.c_str());
  }
  RecordDecl(stmt->name, decl);

  auto expr_ty = InferExpr(stmt->expr, true);
  if (!TryResolve(decl->type, expr_ty) && !TryResolve(expr_ty, decl->type)) {
    throw LocError::Create(
        stmt->expr->Begin(), "mismatched assign type %s = %s",
        ToString(decl->type).c_str(), ToString(expr_ty).c_str());
  }
}

std::shared_ptr<Type> TypeChecker::InferExpr(
    const std::unique_ptr<ast::Expr>& expr, bool as_expr) {
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT: {
      auto& lit = (std::unique_ptr<ast::Lit>&)expr;
      switch (lit->LitKind()) {
        case ast::Lit::Kind::BOOL:
          return RecordType(lit, kTypeBool);
        case ast::Lit::Kind::STRING:
          return RecordType(lit, kTypeString);
        case ast::Lit::Kind::CHAR:
        case ast::Lit::Kind::INT:
          return RecordType(lit, UntypedInt());
        case ast::Lit::Kind::FLOAT:
          return RecordType(lit, UntypedFloat());
      }
    } break;

    case ast::Expr::Kind::CALL: {
      auto& call_expr = (std::unique_ptr<ast::CallExpr>&)expr;
      auto decl = decl_ck.LookupFuncDecl(call_expr->ident->val);
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
        auto arg_ty = InferExpr(arg, true);
        if (!TryResolve(arg_ty, fn_type->args[i])) {
          throw LocError::Create(arg->Begin(), "mismatched arg ty");
        }
      }
      RecordDecl(call_expr->ident, decl);
      return RecordType(call_expr, fn_type->ret);
    } break;

    case ast::Expr::Kind::IDENT: {
      auto& ident = (std::unique_ptr<ast::Ident>&)expr;
      auto decl = decl_ck.LookupVarDecl(ident->val);
      if (decl == nullptr) {
        throw LocError::Create(ident->Begin(), "undefined function %s",
                               ident->val.c_str());
      }
      RecordDecl(ident, decl);
      return RecordType(ident, decl->type);
    } break;

    case ast::Expr::Kind::UNARY: {
      auto& unary_expr = (std::unique_ptr<ast::UnaryExpr>&)expr;
      return RecordType(unary_expr, InferExpr(unary_expr->expr, true));
    } break;

    case ast::Expr::Kind::BINARY: {
      auto& binary = (std::unique_ptr<ast::BinaryExpr>&)expr;
      auto lhs_ty = InferExpr(binary->lhs, true);
      auto rhs_ty = InferExpr(binary->rhs, true);

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
          return RecordType(binary, kTypeBool);

        case ast::BinaryOp::Op::ADD:
        case ast::BinaryOp::Op::SUB:
        case ast::BinaryOp::Op::MUL:
        case ast::BinaryOp::Op::DIV:
          if (!operand_ty->IsUntyped() && !operand_ty->IsFixedNumeric()) {
            throw LocError::Create(binary->Begin(),
                                   "cannot use non numeric type of binary");
          }
          return RecordType(binary, operand_ty);

        case ast::BinaryOp::Op::MOD:
          if (!operand_ty->IsUntyped() && !operand_ty->IsFixedInt()) {
            throw LocError::Create(binary->Begin(),
                                   "cannot use non numeric type of binary");
          }
          return RecordType(binary, operand_ty);
      }
    } break;

    case ast::Expr::Kind::IF:
      return InferIf((std::unique_ptr<ast::If>&)expr, as_expr);
    case ast::Expr::Kind::BLOCK:
      return InferBlock((std::unique_ptr<ast::Block>&)expr, as_expr);

    case ast::Expr::Kind::ARRAY: {
      auto& array = (std::unique_ptr<ast::ArrayExpr>&)expr;
      auto size = array->exprs.size();
      auto elem_ty = Unresolved();
      for (auto& e : array->exprs) {
        auto e_ty = InferExpr(e, true);
        if (!TryResolve(elem_ty, e_ty)) {
          throw LocError::Create(e->Begin(), "mismatch element type");
        }
      }
      return RecordType(array, std::make_shared<ArrayType>(elem_ty, size));
    } break;
  }
}

std::shared_ptr<Type> TypeChecker::InferIf(const std::unique_ptr<ast::If>& e,
                                           bool as_expr) {
  auto cond_ty = InferExpr(e->cond, true);
  if (!TryResolve(cond_ty, kTypeBool)) {
    throw LocError::Create(e->cond->Begin(),
                           "if-statement condition must be bool type");
  }

  auto block_ty = InferBlock(e->block, as_expr);

  std::shared_ptr<Type> whole_ty = kTypeVoid;

  if (e->HasElse()) {
    auto is_then_terminating = e->block->IsTerminating();
    auto is_else_terminating = e->els->IsTerminating();

    std::shared_ptr<Type> els_ty;
    if (e->IsElseIf()) {
      els_ty = InferIf((std::unique_ptr<ast::If>&)e->els, as_expr);
    } else {
      els_ty = InferBlock((std::unique_ptr<ast::Block>&)e->els, as_expr);
    }

    if (as_expr) {
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
      } else if (is_then_terminating) {
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
  } else {
    if (as_expr) {
      // No else if-statement can't be used as value
      //
      // ex) let a = if true { 4 }
      //
      throw LocError::Create(e->Begin(), "incomplete if statement");
    }
  }
  return RecordType(e, whole_ty);
}

std::shared_ptr<Type> TypeChecker::InferBlock(
    const std::unique_ptr<ast::Block>& e, bool as_expr) {
  decl_ck.OpenScope();
  std::shared_ptr<Type> ty = kTypeVoid;
  for (auto it = e->stmts.begin(); it != e->stmts.end(); it++) {
    // If the block will be the value of parent statement,
    // last statement of this block will also be the value.
    // For example, below block will be the value of `let a`
    // and `b` will be a value of whole this block.
    //
    // ```
    // let a = {  // block is expr for `let a`
    //  let b = 2
    //  b         // last statement substitutes expr for block
    // }
    // ```
    bool is_last = std::next(it) == e->stmts.end();
    ty = InferStmt(*it, is_last && as_expr);
  }
  if (!as_expr) {
    ty = kTypeVoid;
  }
  decl_ck.CloseScope();
  return RecordType(e, ty);
}

}  // namespace felis

