#include "type_checker.h"

#include "error/error.h"
#include "macro.h"
#include "string/string.h"

namespace felis {

void TypeChecker::Check(const std::unique_ptr<ast::File>& file) {
  // Insert function decls
  decl_ck_.CheckGlobalLevel(file);

  // Connects node and decl
  for (auto& fn : file->externs) {
    auto decl = decl_ck_.LookupFuncDecl(fn->proto->name->val);
    type_maps_.RecordDecl(fn->proto->name, decl);
  }

  // Delve into each function blocks.
  // Check declaration, infer and check type at the same type.
  for (auto& fn : file->funcs) {
    auto decl = decl_ck_.LookupFuncDecl(fn->proto->name->val);
    type_maps_.RecordDecl(fn->proto->name, decl);
    current_func_ = decl->type;

    decl_ck_.OpenScope();

    for (auto& arg : fn->proto->args->list) {
      if (decl_ck_.ExistsInThisScope(arg->name->val)) {
        throw LocError::Create(arg->name->begin, "redeclared arg name");
      }
      auto arg_decl = std::make_shared<Decl>(
          arg->name->val, decl_ck_.LookupType(arg->type), Decl::Kind::ARG);
      type_maps_.RecordDecl(arg->name, arg_decl);
      decl_ck_.InsertDecl(arg->name->val, arg_decl);
    }

    auto result =
        CheckBlock(fn->block, !current_func_->GetRet()->IsVoid(), false);
    if (result.IsStmt()) {
      // function block ends with non-value statement
      // return type must be void
      if (!current_func_->GetRet()->IsVoid()) {
        throw LocError::Create(fn->block->end, "func type is not void");
      }
    } else if (result.IsExpr()) {
      // function block ends with expression
      // void function discards the value
      if (!current_func_->GetRet()->IsVoid()) {
        // resolve type
        if (!type_maps_.TryResolve(result.type, current_func_->GetRet())) {
          throw LocError::Create(fn->block->end, "mismatch ret type");
        }
      }
    }
  }
}

Eval TypeChecker::CheckBlock(const ast::Block* block, bool needs_type,
                             bool open_scope) {
  if (open_scope) decl_ck_.OpenScope();
  auto result = Eval::Stmt();
  for (auto i = 0; i < block->stmts.size(); ++i) {
    bool is_last = i == block->stmts.size() - 1;
    result = CheckStmt(block->stmts.at(i), is_last && needs_type);
    if (!is_last && result.IsRet()) {
      throw LocError::Create(block->stmts.at(i + 1)->begin, "unreachable code");
    }
  }
  if (open_scope) decl_ck_.CloseScope();
  return type_maps_.RecordResult(block, result);
}

Eval TypeChecker::CheckStmt(const ast::AstNode* stmt, bool needs_type) {
  if (auto ret = node_cast_ornull<ast::RetStmt>(stmt)) {
    return CheckRet(ret);
  } else if (auto var_decl = node_cast_ornull<ast::VarDeclStmt>(stmt)) {
    return CheckVarDecl(var_decl);
  } else if (auto assign = node_cast_ornull<ast::AssignStmt>(stmt)) {
    return CheckAssign(assign);
  } else {
    return CheckExpr(stmt, needs_type);
  }
}

Eval TypeChecker::CheckRet(const ast::RetStmt* stmt) {
  bool is_void_fn = current_func_->GetRet()->IsVoid();
  if (!stmt->expr) {
    if (!is_void_fn) {
      throw LocError::Create(stmt->end, "ret is void");
    }
    return Eval::Ret();
  }

  auto result = CheckExpr(stmt->expr, true);
  if (is_void_fn) {
    if (!result.IsStmt()) {
      throw LocError::Create(stmt->expr->begin, "mismatch ret ty");
    }
  } else {
    if (result.IsStmt()) {
      throw LocError::Create(stmt->expr->begin, "cannot return void type");
    } else if (result.IsExpr()) {
      if (!type_maps_.TryResolve(result.type, current_func_->GetRet())) {
        throw LocError::Create(stmt->expr->begin, "mismatch ret ty");
      }
    }
  }
  return Eval::Ret();
}

Eval TypeChecker::CheckVarDecl(const ast::VarDeclStmt* stmt) {
  // name validation
  auto& name = stmt->name->val;
  if (decl_ck_.ExistsInThisScope(name)) {
    throw LocError::Create(stmt->begin, "redeclared var %s", name.c_str());
  }

  std::shared_ptr<Type> decl_ty = Type::MakeUnresolved();
  if (stmt->type) {
    // has type constraint
    decl_ty = decl_ck_.LookupType(stmt->type);
    if (!decl_ty) {
      throw LocError::Create(stmt->type->begin, "unknown decl type");
    }
  }

  auto stmt_ty = CheckExpr(stmt->expr, true);
  if (!stmt_ty.IsExpr()) {
    throw LocError::Create(stmt->begin, "cannot decl no type var");
  }
  auto expr_ty = stmt_ty.type;

  // resolve type constraints between decl and expr
  if (!type_maps_.TryResolve(decl_ty, expr_ty) &&
      !type_maps_.TryResolve(expr_ty, decl_ty)) {
    throw LocError::Create(stmt->expr->begin, "mismatched decl type %s, %s",
                           ToString(*decl_ty).c_str(),
                           ToString(*expr_ty).c_str());
  }

  auto decl = std::make_shared<Decl>(
      name, decl_ty, stmt->is_let ? Decl::Kind::LET : Decl::Kind::VAR);
  decl_ck_.InsertDecl(name, decl);
  type_maps_.RecordDecl(stmt->name, decl);
  return Eval::Stmt();
}

std::shared_ptr<Type> TypeChecker::GetLValue(const ast::AstNode* node) {
  if (auto ident = node_cast_ornull<ast::Ident>(node)) {
    auto& name = ident->val;
    auto decl = decl_ck_.LookupVarDecl(name);
    if (!decl) {
      throw LocError::Create(node->begin, "undeclared var %s", name.c_str());
    }
    if (!decl->IsAssignable()) {
      throw LocError::Create(node->begin, "%s is declared as mutable variable",
                             name.c_str());
    }
    type_maps_.RecordDecl(ident, decl);

    return decl->type;
  } else if (auto index = node_cast_ornull<ast::Index>(node)) {
    auto expr_ty = GetLValue(index->expr);
    if (!Underlying(expr_ty)->IsArray()) {
      throw LocError::Create(index->expr->begin, "not array type");
    }
    auto idx_eval = CheckExpr(index->idx_expr, true);
    if (!idx_eval.IsExpr()) {
      throw LocError::Create(index->idx_expr->begin, "index is not expr");
    }
    if (Underlying(idx_eval.type)->IsInt()) {
      throw LocError::Create(index->idx_expr->begin, "index is not int");
    }
    return Underlying(expr_ty)->GetArrayElem();
  } else {
    throw LocError::Create(node->begin, "assigning non lvalue expr");
  }
}

Eval TypeChecker::CheckAssign(const ast::AssignStmt* stmt) {
  auto decl_ty = GetLValue(stmt->left);

  auto stmt_ty = CheckExpr(stmt->expr, true);
  if (!stmt_ty.IsExpr()) {
    throw LocError::Create(stmt->begin, "cannot assign no type var");
  }
  auto expr_ty = stmt_ty.type;

  if (!type_maps_.TryResolve(decl_ty, expr_ty) &&
      !type_maps_.TryResolve(expr_ty, decl_ty)) {
    throw LocError::Create(stmt->expr->begin, "mismatched assign type %s = %s ",
                           ToString(*decl_ty).c_str(),
                           ToString(*expr_ty).c_str());
  }
  return Eval::Stmt();
}

Eval TypeChecker::CheckExpr(const ast::AstNode* expr, bool needs_type) {
  if (auto lit = node_cast_ornull<ast::Literal>(expr)) {
    return CheckLit(lit);
  } else if (auto ident = node_cast_ornull<ast::Ident>(expr)) {
    return CheckIdent(ident);
  } else if (auto binary = node_cast_ornull<ast::Binary>(expr)) {
    return CheckBinary(binary);
  } else if (auto unary = node_cast_ornull<ast::Unary>(expr)) {
    return CheckUnary(unary);
  } else if (auto call = node_cast_ornull<ast::Call>(expr)) {
    return CheckCall(call);
  } else if (auto array = node_cast_ornull<ast::Array>(expr)) {
    return CheckArray(array);
  } else if (auto if_stmt = node_cast_ornull<ast::If>(expr)) {
    return CheckIf(if_stmt, needs_type);
  } else if (auto block = node_cast_ornull<ast::Block>(expr)) {
    return CheckBlock(block, needs_type);
  } else if (auto index = node_cast_ornull<ast::Index>(expr)) {
    return CheckIndex(index);
  } else {
    UNREACHABLE
  }
}

Eval TypeChecker::CheckLit(const ast::Literal* lit) {
  std::shared_ptr<Type> ty;
  switch (lit->kind) {
    case ast::Literal::Kind::BOOL:
      ty = Type::MakeBool();
      break;
    case ast::Literal::Kind::STRING:
      ty = Type::MakeString();
      break;
    case ast::Literal::Kind::CHAR:
    case ast::Literal::Kind::INT:
      ty = Type::MakeUntypedInt();
      break;
    case ast::Literal::Kind::FLOAT:
      ty = Type::MakeUntypedFloat();
      break;
  }
  return type_maps_.RecordResult(lit, Eval::Expr(ty));
}

Eval TypeChecker::CheckIdent(const ast::Ident* ident) {
  auto decl = decl_ck_.LookupVarDecl(ident->val);
  if (!decl) {
    throw LocError::Create(ident->begin, "undefined function %s",
                           ident->val.c_str());
  }
  type_maps_.RecordDecl(ident, decl);
  return type_maps_.RecordResult(ident, Eval::Expr(decl->type));
}

Eval TypeChecker::CheckBinary(const ast::Binary* binary) {
  auto lhs_stmt_ty = CheckExpr(binary->lhs, true);
  if (!lhs_stmt_ty.IsExpr()) {
    throw LocError::Create(binary->lhs->begin, "non type lhs ty");
  }
  auto lhs_ty = lhs_stmt_ty.type;
  auto rhs_stmt_ty = CheckExpr(binary->rhs, true);
  if (!rhs_stmt_ty.IsExpr()) {
    throw LocError::Create(binary->rhs->begin, "non type rhs ty");
  }
  auto rhs_ty = rhs_stmt_ty.type;

  std::shared_ptr<Type> operand_ty;
  if (type_maps_.TryResolve(lhs_ty, rhs_ty)) {
    operand_ty = rhs_ty;
  } else if (type_maps_.TryResolve(rhs_ty, lhs_ty)) {
    operand_ty = lhs_ty;
  } else {
    throw LocError::Create(binary->lhs->begin, "unmatch type %s, %s",
                           ToString(*lhs_ty).c_str(),
                           ToString(*rhs_ty).c_str());
  }

  std::shared_ptr<Type> ty;
  switch (binary->op->kind) {
    case ast::BinaryOp::Kind::EQEQ:
    case ast::BinaryOp::Kind::NEQ:
    case ast::BinaryOp::Kind::LT:
    case ast::BinaryOp::Kind::LE:
    case ast::BinaryOp::Kind::GT:
    case ast::BinaryOp::Kind::GE:
      if (operand_ty->IsString() || operand_ty->IsVoid()) {
        throw LocError::Create(binary->begin,
                               "cannot use this type for comparison");
      }
      ty = Type::MakeBool();
      break;

    case ast::BinaryOp::Kind::ADD:
    case ast::BinaryOp::Kind::SUB:
    case ast::BinaryOp::Kind::MUL:
    case ast::BinaryOp::Kind::DIV:
      if (!operand_ty->IsUntype() && !operand_ty->IsNum()) {
        throw LocError::Create(binary->begin,
                               "cannot use non numeric type of binary");
      }
      ty = operand_ty;
      break;

    case ast::BinaryOp::Kind::MOD:
      if (!operand_ty->IsUntype() && !operand_ty->IsInt()) {
        throw LocError::Create(binary->begin,
                               "cannot use non numeric type of binary");
      }
      ty = operand_ty;
      break;
  }
  return type_maps_.RecordResult(binary, Eval::Expr(ty));
}

Eval TypeChecker::CheckUnary(const ast::Unary* unary) {
  auto stmt_ty = CheckExpr(unary->expr, true);
  if (!stmt_ty.IsExpr()) {
    throw LocError::Create(unary->begin, "non type unary ty");
  }
  return type_maps_.RecordResult(unary, Eval::Expr(stmt_ty.type));
}

Eval TypeChecker::CheckCall(const ast::Call* call) {
  auto decl = decl_ck_.LookupFuncDecl(call->ident->val);
  if (decl == nullptr) {
    throw LocError::Create(call->begin, "undefined function %s",
                           call->ident->val.c_str());
  }
  auto fn_type = decl->type;
  if (fn_type->GetArgs().size() != call->args.size()) {
    throw LocError::Create(call->begin, "args count doesn't match");
  }

  for (auto i = 0; i < call->args.size(); ++i) {
    auto& arg = call->args.at(i);
    auto stmt_ty = CheckExpr(arg, true);
    if (!stmt_ty.IsExpr()) {
      throw LocError::Create(arg->begin, "non type arg ty");
    }
    auto arg_ty = stmt_ty.type;
    if (!type_maps_.TryResolve(arg_ty, fn_type->GetArgs().at(i))) {
      throw LocError::Create(arg->begin, "mismatched arg ty");
    }
  }
  type_maps_.RecordDecl(call->ident, decl);
  return type_maps_.RecordResult(call, Eval::Expr(fn_type->GetRet()));
}

Eval TypeChecker::CheckArray(const ast::Array* array) {
  auto size = array->exprs.size();
  auto elem_ty = Type::MakeUnresolved();
  for (auto& expr : array->exprs) {
    auto stmt_ty = CheckExpr(expr, true);
    if (!stmt_ty.IsExpr()) {
      throw LocError::Create(expr->begin, "array expr not type");
    }
    if (!type_maps_.TryResolve(elem_ty, stmt_ty.type)) {
      throw LocError::Create(expr->begin, "mismatch element type");
    }
    elem_ty = stmt_ty.type;
  }
  return type_maps_.RecordResult(array,
                                 Eval::Expr(Type::MakeArray(elem_ty, size)));
}

Eval TypeChecker::CheckIndex(const ast::Index* index) {
  auto expr_res = CheckExpr(index->expr, true);
  if (!expr_res.IsExpr()) {
    throw LocError::Create(index->expr->begin, "index expr not type");
  }
  if (!Underlying(expr_res.type)->IsArray()) {
    throw LocError::Create(index->expr->begin, "not array");
  }
  auto idx_res = CheckExpr(index->idx_expr, true);
  if (!idx_res.IsExpr()) {
    throw LocError::Create(index->expr->begin, "index idx not type");
  }
  return type_maps_.RecordResult(
      index, Eval::Expr(Underlying(expr_res.type)->GetArrayElem()));
}

Eval TypeChecker::CheckIf(const ast::If* e, bool needs_type) {
  auto cond_stmt_ty = CheckExpr(e->cond, true);
  if (!cond_stmt_ty.IsExpr()) {
    throw LocError::Create(e->cond->begin, "cond is not type");
  }
  auto cond_ty = cond_stmt_ty.type;
  if (!type_maps_.TryResolve(cond_ty, Type::MakeBool())) {
    throw LocError::Create(e->cond->begin,
                           "if-statement condition must be bool type");
  }

  auto block_stmt_ty = CheckBlock(e->block, true);

  if (!e->HasElse()) {
    // Incomp
    return type_maps_.RecordResult(e, Eval::Stmt());
  }

  // comp
  auto els_stmt_ty = Eval::Stmt();
  if (e->IsElseIf()) {
    els_stmt_ty = CheckIf((ast::If*)e->els, true);
  } else {
    els_stmt_ty = CheckBlock((ast::Block*)e->els, true);
  }

  if (block_stmt_ty.IsStmt() || els_stmt_ty.IsStmt()) {
    // If one of blocks is STMT, if-stmt becomes STMT as a whole.
    //   if true {
    //     let a = 4  // STMT
    //   } else {
    //     3          // EXPR
    //   }
    return type_maps_.RecordResult(e, Eval::Stmt());
  }

  if (block_stmt_ty.IsRet() && els_stmt_ty.IsRet()) {
    return type_maps_.RecordResult(e, Eval::Ret());
  }

  if (!needs_type) {
    return type_maps_.RecordResult(e, Eval::Stmt());
  }

  std::shared_ptr<Type> whole_ty;
  if (block_stmt_ty.IsRet()) {
    whole_ty = els_stmt_ty.type;
  } else if (els_stmt_ty.IsRet()) {
    whole_ty = block_stmt_ty.type;
  } else {
    // expr and expr
    // resolve type
    auto block_ty = block_stmt_ty.type;
    auto els_ty = els_stmt_ty.type;
    if (type_maps_.TryResolve(block_ty, els_ty)) {
      whole_ty = els_ty;
    } else if (type_maps_.TryResolve(els_ty, block_ty)) {
      whole_ty = block_ty;
    } else {
      throw LocError::Create(e->begin, "unmatched els branches types");
    }
  }
  return type_maps_.RecordResult(e, Eval::Expr(whole_ty));
}

}  // namespace felis
