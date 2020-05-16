#include "check/decl_checker.h"

#include "error/error.h"
#include "macro.h"
#include "unique.h"

namespace felis {

void DeclChecker::SetupBuiltin() {
  assert(current_scope_->IsTop());
  // insert basic types into global scope
  current_scope_->InsertType("void", std::make_shared<Type>(Type::Kind::VOID));
  current_scope_->InsertType("i32", std::make_shared<Type>(Type::Kind::I32));
  current_scope_->InsertType("i64", std::make_shared<Type>(Type::Kind::I64));
  current_scope_->InsertType("f32", std::make_shared<Type>(Type::Kind::F32));
  current_scope_->InsertType("f64", std::make_shared<Type>(Type::Kind::F64));
  current_scope_->InsertType("bool", std::make_shared<Type>(Type::Kind::BOOL));
  current_scope_->InsertType("char", std::make_shared<Type>(Type::Kind::CHAR));
  current_scope_->InsertType("string",
                             std::make_shared<Type>(Type::Kind::STRING));
}

void DeclChecker::CheckStmt(const std::unique_ptr<ast::Stmt>& stmt) {
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      CheckExpr((std::unique_ptr<ast::Expr>&)stmt);
      break;
    case ast::Stmt::Kind::RET:
      CheckRet((std::unique_ptr<ast::RetStmt>&)stmt);
      break;
    case ast::Stmt::Kind::VAR_DECL:
      CheckVarDecl((std::unique_ptr<ast::VarDeclStmt>&)stmt);
      break;
    case ast::Stmt::Kind::ASSIGN:
      CheckAssign((std::unique_ptr<ast::AssignStmt>&)stmt);
      break;
  }
}

void DeclChecker::CheckExpr(const std::unique_ptr<ast::Expr>& expr) {
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT:
      /* return MakeLit(unique_cast<ast::Lit>(std::move(expr))); */
      break;

    case ast::Expr::Kind::CALL: {
      auto& call_expr = (std::unique_ptr<ast::CallExpr>&)expr;
      auto begin = call_expr->Begin();

      auto decl = LookupDecl(call_expr->ident->val);
      if (decl == nullptr) {
        throw LocError::Create(begin, "undefined function %s",
                               call_expr->ident->val.c_str());
      }
      if (!decl->IsFunc()) {
        throw LocError::Create(begin, "%s is not declared as function",
                               call_expr->ident->val.c_str());
      }
      auto fn_type = (FuncType*)decl->type.get();
      if (fn_type->args.size() != call_expr->args.size()) {
        throw LocError::Create(begin, "args count doesn't match");
      }

      for (auto& arg : call_expr->args) {
        CheckExpr(arg);
      }
    } break;

    case ast::Expr::Kind::IDENT: {
      auto& ident = (std::unique_ptr<ast::Ident>&)expr;
      auto begin = ident->Begin();
      auto decl = LookupDecl(ident->val);
      if (decl == nullptr) {
        throw LocError::Create(begin, "undefined function %s",
                               ident->val.c_str());
      }
      if (decl->IsFunc()) {
        throw LocError::Create(begin, "%s is not declared as variable",
                               ident->val.c_str());
      }
    } break;

    case ast::Expr::Kind::UNARY: {
      auto& unary_expr = (std::unique_ptr<ast::UnaryExpr>&)expr;
      auto begin = unary_expr->Begin();
      CheckExpr(unary_expr->expr);
    } break;

    case ast::Expr::Kind::BINARY: {
      auto& binary = (std::unique_ptr<ast::BinaryExpr>&)expr;
      auto begin = binary->Begin();
      CheckExpr(binary->lhs);
      CheckExpr(binary->rhs);
    } break;
    case ast::Expr::Kind::IF:
      return CheckIf((std::unique_ptr<ast::If>&)expr);
    case ast::Expr::Kind::BLOCK:
      return CheckBlock((std::unique_ptr<ast::Block>&)expr);
  }
}

void DeclChecker::CheckRet(const std::unique_ptr<ast::RetStmt>& stmt) {
  auto func_type = current_func_->AsFuncType();
  bool is_void_func = func_type->ret->IsVoid();
  bool has_ret_expr = stmt->expr != nullptr;
  if (has_ret_expr && is_void_func) {
    throw LocError::Create(stmt->expr->Begin(), "func type is void");
  } else if (!has_ret_expr && !is_void_func) {
    throw LocError::Create(stmt->Begin(), "func type is not void");
  }
}

void DeclChecker::CheckVarDecl(const std::unique_ptr<ast::VarDeclStmt>& stmt) {
  std::string name = stmt->name->val;
  if (!CanDecl(name)) {
    throw LocError::Create(stmt->Begin(), "redeclared var %s", name.c_str());
  }
  auto decl = std::make_shared<Decl>(
      name, std::make_shared<Type>(Type::Kind::UNRESOLVED),
      stmt->is_let ? Decl::Kind::LET : Decl::Kind::VAR);
  current_scope_->InsertDecl(name, decl);
  ast_decl_[stmt.get()] = decl;
}

void DeclChecker::CheckAssign(const std::unique_ptr<ast::AssignStmt>& stmt) {
  std::string name = stmt->name->val;
  auto decl = LookupDecl(name);
  if (!decl) {
    throw LocError::Create(stmt->Begin(), "undeclared var %s", name.c_str());
  }
  if (decl->IsFunc()) {
    throw LocError::Create(stmt->Begin(), "%s is declared as function",
                           name.c_str());
  }
  if (!decl->IsAssignable()) {
    throw LocError::Create(stmt->Begin(), "%s is declared as mutable variable",
                           name.c_str());
  }
  CheckExpr(stmt->expr);
}

void DeclChecker::CheckIf(const std::unique_ptr<ast::If>& stmt) {
  CheckExpr(stmt->cond);
  CheckBlock(stmt->block);

  if (stmt->HasElse()) {
    if (stmt->IsElseIf()) {
      CheckIf((std::unique_ptr<ast::If>&)stmt->els);
    } else if (stmt->IsElseBlock()) {
      CheckBlock((std::unique_ptr<ast::Block>&)stmt->els);
    }
  }
}

void DeclChecker::CheckBlock(const std::unique_ptr<ast::Block>& block) {
  OpenScope();
  for (auto& stmt : block->stmts) {
    CheckStmt(stmt);
  }
  CloseScope();
}

void DeclChecker::Check(const std::unique_ptr<ast::File>& file) {
  for (auto& ext : file->externs) {
    auto decl = InsertFnDecl(true, ext->proto);
    ast_decl_[ext.get()] = decl;
  }
  for (auto& fn : file->fn_decls) {
    auto decl = InsertFnDecl(false, fn->proto);
    ast_decl_[fn.get()] = decl;
  }

  for (auto& fn : file->fn_decls) {
    CheckFnDecl(fn);
  }
}

void DeclChecker::CheckFnDecl(const std::unique_ptr<ast::FnDecl>& fn) {
  current_func_ = ast_decl_[fn.get()];
  OpenScope();
  for (auto& arg : fn->proto->args->list) {
    // arg-name duplication is already checked in parser
    auto decl = std::make_shared<Decl>(arg->name->val, LookupType(arg->ty->val),
                                       Decl::Kind::ARG);
    current_scope_->InsertDecl(arg->name->val, decl);
    ast_decl_[arg.get()] = decl;
  }
  for (auto& stmt : fn->block->stmts) {
    CheckStmt(stmt);
  }
  CloseScope();

  // TODO
  /* if (!block->IsTerminating()) { */
  /*   if (current_func_->AsFuncType()->ret->IsVoid()) { */
  /*     block->stmts.push_back(std::make_unique<hir::RetStmt>(block->End() -
   * 1)); */
  /*   } else { */
  /*     throw CompileError::Create("func %s is not terminated", */
  /*                                hirDecl->decl->name.c_str()); */
  /*   } */
  /* } */
}

std::shared_ptr<Decl> DeclChecker::InsertFnDecl(
    bool is_ext, const std::unique_ptr<ast::FnProto>& proto) {
  if (!CanDecl(proto->name->val)) {
    throw LocError::Create(proto->name->Begin(), "redeclared function %s",
                           proto->name->val.c_str());
  }

  std::vector<std::shared_ptr<Type>> args;
  for (auto& arg : proto->args->list) {
    auto ty = LookupType(arg->ty->val);
    if (!ty) {
      throw LocError::Create(arg->ty->Begin(), "unknown arg type %s",
                             arg->ty->val.c_str());
    }
    args.push_back(ty);
  }
  std::shared_ptr<Type> ret_ty;
  if (proto->ret) {
    ret_ty = LookupType(proto->ret->val);
    if (!ret_ty) {
      throw LocError::Create(proto->ret->Begin(), "unknown ret type %s",
                             proto->ret->val.c_str());
    }
  } else {
    ret_ty = std::make_shared<Type>(Type::Kind::VOID);
  }
  auto fn_type = std::make_shared<FuncType>(std::move(args), std::move(ret_ty));
  Decl::Kind kind = is_ext ? Decl::Kind::EXT : Decl::Kind::FN;
  auto decl = std::make_shared<Decl>(proto->name->val, fn_type, kind);
  current_scope_->InsertDecl(proto->name->val, decl);
  return decl;
}

void DeclChecker::OpenScope() {
  current_scope_ = std::make_shared<Scope>(current_scope_);
}

void DeclChecker::CloseScope() {
  assert(!current_scope_->IsTop());
  current_scope_ = current_scope_->GetParent();
}

bool DeclChecker::CanDecl(std::string name) {
  return current_scope_->FindDecl(name) == nullptr;
}

void DeclChecker::DebugScope() {
  std::cout << "----------------" << std::endl;
  auto scope = current_scope_;
  int i = 0;
  while (scope) {
    std::cout << "Scope " << i++ << (scope->IsTop() ? "(Top)" : "")
              << std::endl;
    scope->Debug();
    scope = scope->GetParent();
  }
  std::cout << "----------------" << std::endl;
}

std::shared_ptr<Decl> DeclChecker::LookupDecl(std::string name) {
  auto scope = current_scope_;
  while (scope) {
    auto def = scope->FindDecl(name);
    if (def) return def;
    scope = scope->GetParent();
  }
  return nullptr;
}

std::shared_ptr<Type> DeclChecker::LookupType(std::string name) {
  auto scope = current_scope_;
  while (scope) {
    auto ty = scope->FindType(name);
    if (ty) return ty;
    scope = scope->GetParent();
  }
  return nullptr;
}

}  // namespace felis
