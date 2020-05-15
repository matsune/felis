#include "check/check.h"

#include <assert.h>

#include "error/error.h"
#include "macro.h"
#include "unique.h"

namespace felis {

void Checker::SetupBuiltin() {
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

std::unique_ptr<hir::File> Checker::Check(std::unique_ptr<ast::File> file) {
  auto hir_file = std::make_unique<hir::File>();
  for (auto& ext : file->externs) {
    auto decl = InsertFnDecl(true, ext->proto);
    hir_file->externs.emplace_back(
        new hir::Extern(ext->Begin(), ext->End(), decl));
  }
  for (auto& fn : file->fn_decls) {
    auto decl = InsertFnDecl(false, fn->proto);
    hir_file->fn_decls.push_back(
        std::make_unique<hir::FnDecl>(fn->Begin(), decl));
  }

  int i = 0;
  while (!file->fn_decls.empty()) {
    auto fn = file->fn_decls.move_front();
    hir_file->fn_decls[i]->block =
        CheckFnDecl(std::move(fn), hir_file->fn_decls[i]);
    i++;
  }
  return std::move(hir_file);
}

std::unique_ptr<hir::Block> Checker::CheckFnDecl(
    std::unique_ptr<ast::FnDecl> fn_decl,
    std::unique_ptr<hir::FnDecl>& hirDecl) {
  current_func_ = hirDecl->decl;
  OpenScope();
  for (auto& arg : fn_decl->proto->args->list) {
    // arg-name duplication is already checked in parser
    auto argDecl = std::make_shared<Decl>(
        arg->name->val, LookupType(arg->ty->val), Decl::Kind::ARG);
    current_scope_->InsertDecl(arg->name->val, argDecl);
    hirDecl->args.push_back(argDecl);
  }
  auto block = CheckBlock(std::move(fn_decl->block), true);
  CloseScope();

  if (!block->IsTerminating()) {
    if (current_func_->AsFuncType()->ret->IsVoid()) {
      block->stmts.push_back(std::make_unique<hir::RetStmt>(block->End() - 1));
    } else {
      throw CompileError::Create("func %s is not terminated",
                                 hirDecl->decl->name.c_str());
    }
  }

  return std::move(block);
}

std::unique_ptr<hir::Stmt> Checker::CheckStmt(std::unique_ptr<ast::Stmt> stmt) {
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR: {
      return MakeExpr(unique_cast<ast::Stmt, ast::Expr>(std::move(stmt)));
    } break;
    case ast::Stmt::Kind::RET:
      return CheckRetStmt(
          unique_cast<ast::Stmt, ast::RetStmt>(std::move(stmt)));
    case ast::Stmt::Kind::VAR_DECL:
      return CheckVarDeclStmt(
          unique_cast<ast::Stmt, ast::VarDeclStmt>(std::move(stmt)));
    case ast::Stmt::Kind::ASSIGN:
      return CheckAssignStmt(
          unique_cast<ast::Stmt, ast::AssignStmt>(std::move(stmt)));
    case ast::Stmt::Kind::IF:
      return CheckIfStmt(unique_cast<ast::Stmt, ast::IfStmt>(std::move(stmt)));
    case ast::Stmt::Kind::BLOCK:
      return CheckBlock(unique_cast<ast::Stmt, ast::Block>(std::move(stmt)));
  }
}

std::unique_ptr<hir::RetStmt> Checker::CheckRetStmt(
    std::unique_ptr<ast::RetStmt> ret_stmt) {
  auto begin = ret_stmt->Begin();
  auto func_type = current_func_->AsFuncType();
  if (ret_stmt->expr) {
    auto expr = MakeExpr(std::move(ret_stmt->expr));
    if (*expr->Ty() != *func_type->ret)
      expr = TryExprTy(std::move(expr), func_type->ret);
    /* expr->Debug(); */
    return std::make_unique<hir::RetStmt>(begin, std::move(expr));
  } else {
    // empty return
    if (!func_type->ret->IsVoid()) {
      throw LocError::Create(ret_stmt->Begin(), "func type is not void");
    }
    return std::make_unique<hir::RetStmt>(begin);
  }
}

std::unique_ptr<hir::VarDeclStmt> Checker::CheckVarDeclStmt(
    std::unique_ptr<ast::VarDeclStmt> decl_stmt) {
  auto begin = decl_stmt->Begin();
  std::string name = decl_stmt->name->val;
  if (!CanDecl(name)) {
    throw LocError::Create(decl_stmt->Begin(), "redeclared var %s",
                           name.c_str());
  }
  auto exp = MakeExpr(std::move(decl_stmt->expr));
  auto decl = std::make_shared<Decl>(
      name, exp->Ty(), decl_stmt->is_let ? Decl::Kind::LET : Decl::Kind::VAR);
  current_scope_->InsertDecl(name, decl);
  /* decl->Debug(); */
  /* DebugScope(); */
  return std::make_unique<hir::VarDeclStmt>(begin, decl, std::move(exp));
}

std::unique_ptr<hir::AssignStmt> Checker::CheckAssignStmt(
    std::unique_ptr<ast::AssignStmt> assign_stmt) {
  auto begin = assign_stmt->Begin();
  auto name = assign_stmt->name->val;
  auto decl = LookupDecl(name);
  if (!decl) {
    throw LocError::Create(assign_stmt->Begin(), "undeclared var %s",
                           name.c_str());
  }
  if (decl->IsFunc()) {
    throw LocError::Create(assign_stmt->Begin(), "%s is declared as function",
                           name.c_str());
  }
  if (!decl->IsAssignable()) {
    throw LocError::Create(assign_stmt->Begin(),
                           "%s is declared as mutable variable", name.c_str());
  }
  auto expr = MakeExpr(std::move(assign_stmt->expr));
  expr = TryExprTy(std::move(expr), decl->type);
  return std::make_unique<hir::AssignStmt>(begin, decl, std::move(expr));
}

std::unique_ptr<hir::IfStmt> Checker::CheckIfStmt(
    std::unique_ptr<ast::IfStmt> if_stmt) {
  auto begin = if_stmt->Begin();
  auto cond = MakeExpr(std::move(if_stmt->cond));
  if (!cond->Ty()->IsBool()) {
    throw LocError::Create(cond->Begin(), "non bool if cond");
  }
  auto block = CheckBlock(std::move(if_stmt->block));

  if (if_stmt->els) {
    if (if_stmt->els->StmtKind() == ast::Stmt::Kind::IF) {
      auto els = CheckIfStmt(
          unique_cast<ast::Stmt, ast::IfStmt>(std::move(if_stmt->els)));
      return std::make_unique<hir::IfStmt>(begin, std::move(cond),
                                           std::move(block), std::move(els));
    } else if (if_stmt->els->StmtKind() == ast::Stmt::Kind::BLOCK) {
      auto els = CheckBlock(
          unique_cast<ast::Stmt, ast::Block>(std::move(if_stmt->els)));
      return std::make_unique<hir::IfStmt>(begin, std::move(cond),
                                           std::move(block), std::move(els));
    }
  }
  return std::make_unique<hir::IfStmt>(begin, std::move(cond),
                                       std::move(block));
}

std::unique_ptr<hir::Block> Checker::CheckBlock(
    std::unique_ptr<ast::Block> block, bool is_fn_body) {
  if (!is_fn_body) OpenScope();
  auto begin = block->Begin();
  auto end = block->End();

  unique_deque<hir::Stmt> stmts;
  while (!block->stmts.empty()) {
    auto stmt_ast = block->stmts.move_front();

    bool is_last = block->stmts.empty();
    auto stmt = CheckStmt(std::move(stmt_ast));
    bool is_terminating = stmt->IsTerminating();

    if (is_terminating && !is_last) {
      auto& next_stmt = block->stmts.front();
      throw LocError::Create(next_stmt->Begin(), "unreachable code");
    }

    stmts.push_back(std::move(stmt));
  }

  if (!is_fn_body) CloseScope();

  return std::make_unique<hir::Block>(begin, end, std::move(stmts));
}

hir::Unary::Op un_op_ast_to_hir(ast::UnaryOp::Op op) {
  switch (op) {
    case ast::UnaryOp::Op::NEG:
      return hir::Unary::Op::NEG;
    case ast::UnaryOp::Op::NOT:
      return hir::Unary::Op::NOT;
  }
}

hir::Binary::Op bin_op_ast_to_hir(ast::BinaryOp::Op op) {
  switch (op) {
    case ast::BinaryOp::Op::LT:
      return hir::Binary::Op::LT;
    case ast::BinaryOp::Op::LE:
      return hir::Binary::Op::LE;
    case ast::BinaryOp::Op::GT:
      return hir::Binary::Op::GT;
    case ast::BinaryOp::Op::GE:
      return hir::Binary::Op::GE;
    case ast::BinaryOp::Op::ADD:
      return hir::Binary::Op::ADD;
    case ast::BinaryOp::Op::SUB:
      return hir::Binary::Op::SUB;
    case ast::BinaryOp::Op::MUL:
      return hir::Binary::Op::MUL;
    case ast::BinaryOp::Op::DIV:
      return hir::Binary::Op::DIV;
    case ast::BinaryOp::Op::MOD:
      return hir::Binary::Op::MOD;
  }
}

std::unique_ptr<hir::Expr> Checker::MakeExpr(std::unique_ptr<ast::Expr> expr) {
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT: {
      return MakeLit(unique_cast<ast::Expr, ast::Lit>(std::move(expr)));
    }
    case ast::Expr::Kind::CALL: {
      auto call_expr = unique_cast<ast::Expr, ast::CallExpr>(std::move(expr));
      auto begin = call_expr->Begin();
      auto end = call_expr->End();

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
      auto call = std::make_unique<hir::Call>(begin, end);
      call->decl = decl;

      int i = 0;
      while (!call_expr->args.empty()) {
        auto arg = call_expr->args.move_front();

        auto exp = MakeExpr(std::move(arg));
        auto ty = fn_type->args[i];
        exp = TryExprTy(std::move(exp), ty);
        call->args.push_back(std::move(exp));
        i++;
      }
      return call;
    } break;

    case ast::Expr::Kind::IDENT: {
      auto ident = unique_cast<ast::Expr, ast::Ident>(std::move(expr));
      auto begin = ident->Begin();
      auto end = ident->End();
      auto decl = LookupDecl(ident->val);
      if (decl == nullptr) {
        throw LocError::Create(begin, "undefined function %s",
                               ident->val.c_str());
      }
      if (decl->IsFunc()) {
        throw LocError::Create(begin, "%s is not declared as variable",
                               ident->val.c_str());
      }
      auto value = std::make_unique<hir::Variable>(begin, end);
      value->decl = decl;
      return value;

    } break;

    case ast::Expr::Kind::UNARY: {
      auto unary_expr = unique_cast<ast::Expr, ast::UnaryExpr>(std::move(expr));
      auto begin = unary_expr->Begin();
      auto end = unary_expr->End();
      auto op = un_op_ast_to_hir(unary_expr->op->op);
      auto e = MakeExpr(std::move(unary_expr->expr));
      if (e->IsConstant()) {
        return MakeConstUnary(
            unique_cast<hir::Expr, hir::Constant>(std::move(e)),
            un_op_ast_to_hir(unary_expr->op->op));
      }
      return std::make_unique<hir::Unary>(begin, end, op, std::move(e));
    } break;

    case ast::Expr::Kind::BINARY: {
      auto binary = unique_cast<ast::Expr, ast::BinaryExpr>(std::move(expr));
      auto begin = binary->Begin();
      auto end = binary->End();
      auto op = bin_op_ast_to_hir(binary->op->op);
      auto lhs = MakeExpr(std::move(binary->lhs));
      auto rhs = MakeExpr(std::move(binary->rhs));
      if (lhs->IsConstant() && rhs->IsConstant()) {
        return MakeConstBinary(
            unique_cast<hir::Expr, hir::Constant>(std::move(lhs)),
            unique_cast<hir::Expr, hir::Constant>(std::move(rhs)),
            bin_op_ast_to_hir(binary->op->op));
      }
      return CheckBinary(std::make_unique<hir::Binary>(
          begin, end, op, std::move(lhs), std::move(rhs)));
    } break;
    default:
      return nullptr;
  }
}

std::unique_ptr<hir::Constant> Checker::MakeConstUnary(
    std::unique_ptr<hir::Constant> cons, hir::Unary::Op op) {
  switch (op) {
    case hir::Unary::Op::NOT:
      if (cons->Ty()->IsBool()) {
        auto b = unique_cast<hir::Constant, hir::BoolConstant>(std::move(cons));
        b->val = !b->val;
        return std::move(b);
      } else {
        throw LocError::Create(cons->Begin(), "non bool type");
      }
    case hir::Unary::Op::NEG:
      if (cons->Ty()->IsNumeric()) {
        if (cons->Ty()->IsI32() || cons->Ty()->IsI64()) {
          auto b =
              unique_cast<hir::Constant, hir::IntConstant>(std::move(cons));
          b->val = -b->val;
          return b;
        } else {
          auto b =
              unique_cast<hir::Constant, hir::FloatConstant>(std::move(cons));
          b->val = -b->val;
          return b;
        }
      } else {
        throw LocError::Create(cons->Begin(), "non numeric type");
      }
  }
}

std::unique_ptr<hir::Constant> Checker::MakeConstBinary(
    std::unique_ptr<hir::Constant> lhs, std::unique_ptr<hir::Constant> rhs,
    hir::Binary::Op op) {
  auto lhs_begin = lhs->Begin();
  auto rhs_end = rhs->End();
  switch (lhs->ConstantKind()) {
    case hir::Constant::Kind::INT: {
      auto l = unique_cast<hir::Constant, hir::IntConstant>(std::move(lhs));
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // int int
          auto r = unique_cast<hir::Constant, hir::IntConstant>(std::move(rhs));
          switch (op) {
            case hir::Binary::Op::LT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val < r->val);
            case hir::Binary::Op::LE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val <= r->val);
            case hir::Binary::Op::GT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val > r->val);
            case hir::Binary::Op::GE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val >= r->val);

            case hir::Binary::Op::ADD:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val + r->val);
            case hir::Binary::Op::SUB:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val - r->val);

            case hir::Binary::Op::MUL:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val * r->val);
            case hir::Binary::Op::DIV:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val / r->val);
            case hir::Binary::Op::MOD:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val % r->val);
          }
        } break;

        case hir::Constant::Kind::FLOAT: {  // int float
          auto r =
              unique_cast<hir::Constant, hir::FloatConstant>(std::move(rhs));
          switch (op) {
            case hir::Binary::Op::LT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val < r->val);
            case hir::Binary::Op::LE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val <= r->val);
            case hir::Binary::Op::GT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val > r->val);
            case hir::Binary::Op::GE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val >= r->val);

            case hir::Binary::Op::ADD:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val + r->val, true);
            case hir::Binary::Op::SUB:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val - r->val, true);

            case hir::Binary::Op::MUL:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val * r->val, true);
            case hir::Binary::Op::DIV:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val / r->val, true);
            case hir::Binary::Op::MOD:
              throw LocError::Create(
                  r->Begin(), "operator \% not defined on untyped float");
          }
        } break;

        case hir::Constant::Kind::CHAR: {  // int char
          auto r =
              unique_cast<hir::Constant, hir::CharConstant>(std::move(rhs));
          switch (op) {
            case hir::Binary::Op::LT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val < r->val);
            case hir::Binary::Op::LE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val <= r->val);
            case hir::Binary::Op::GT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val > r->val);
            case hir::Binary::Op::GE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val >= r->val);

            case hir::Binary::Op::ADD:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val + r->val);
            case hir::Binary::Op::SUB:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val - r->val);

            case hir::Binary::Op::MUL:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val * r->val);
            case hir::Binary::Op::DIV:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val / r->val);
            case hir::Binary::Op::MOD:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val % r->val);
          }
        } break;
        default:
          throw LocError::Create(lhs->Begin(), "cannot binary");
      }
    } break;

    case hir::Constant::Kind::FLOAT: {
      auto l = unique_cast<hir::Constant, hir::FloatConstant>(std::move(lhs));
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // float int
          auto r = unique_cast<hir::Constant, hir::IntConstant>(std::move(rhs));
          switch (op) {
            case hir::Binary::Op::LT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val < r->val);
            case hir::Binary::Op::LE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val <= r->val);
            case hir::Binary::Op::GT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val > r->val);
            case hir::Binary::Op::GE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val >= r->val);

            case hir::Binary::Op::ADD:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val + r->val, true);
            case hir::Binary::Op::SUB:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val - r->val, true);

            case hir::Binary::Op::MUL:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val * r->val, true);
            case hir::Binary::Op::DIV:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val / r->val, true);
            case hir::Binary::Op::MOD:
              throw LocError::Create(
                  l->Begin(), "operator \% not defined on untyped float");
          }
        } break;
        case hir::Constant::Kind::FLOAT: {  // float float
          auto r =
              unique_cast<hir::Constant, hir::FloatConstant>(std::move(rhs));
          bool is_32 = l->is_32 && r->is_32;
          switch (op) {
            case hir::Binary::Op::LT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val < r->val);
            case hir::Binary::Op::LE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val <= r->val);
            case hir::Binary::Op::GT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val > r->val);
            case hir::Binary::Op::GE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val >= r->val);

            case hir::Binary::Op::ADD:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val + r->val, is_32);
            case hir::Binary::Op::SUB:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val - r->val, is_32);

            case hir::Binary::Op::MUL:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val * r->val, is_32);
            case hir::Binary::Op::DIV:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val / r->val, is_32);
            case hir::Binary::Op::MOD:
              throw LocError::Create(
                  l->Begin(), "operator \% not defined on untyped float");
          }
        } break;

        case hir::Constant::Kind::CHAR: {  // float char
          auto r =
              unique_cast<hir::Constant, hir::CharConstant>(std::move(rhs));
          switch (op) {
            case hir::Binary::Op::LT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val < r->val);
            case hir::Binary::Op::LE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val <= r->val);
            case hir::Binary::Op::GT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val > r->val);
            case hir::Binary::Op::GE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val >= r->val);

            case hir::Binary::Op::ADD:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val + r->val, true);
            case hir::Binary::Op::SUB:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val - r->val, true);

            case hir::Binary::Op::MUL:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val * r->val, true);
            case hir::Binary::Op::DIV:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val / r->val, true);
            case hir::Binary::Op::MOD:
              throw LocError::Create(
                  l->Begin(), "operator \% not defined on untyped float");
          }
        } break;
        default:
          throw LocError::Create(lhs->Begin(), "cannot binary");
      }
    } break;

    case hir::Constant::Kind::CHAR: {
      auto l = unique_cast<hir::Constant, hir::CharConstant>(std::move(lhs));
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // char int
          auto r = unique_cast<hir::Constant, hir::IntConstant>(std::move(rhs));
          switch (op) {
            case hir::Binary::Op::LT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val < r->val);
            case hir::Binary::Op::LE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val <= r->val);
            case hir::Binary::Op::GT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val > r->val);
            case hir::Binary::Op::GE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val >= r->val);

            case hir::Binary::Op::ADD:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val + r->val);
            case hir::Binary::Op::SUB:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val - r->val);

            case hir::Binary::Op::MUL:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val * r->val);
            case hir::Binary::Op::DIV:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val / r->val);
            case hir::Binary::Op::MOD:
              return std::make_unique<hir::IntConstant>(lhs_begin, rhs_end,
                                                        l->val % r->val);
          }
        } break;
        case hir::Constant::Kind::FLOAT: {  // char float
          auto r =
              unique_cast<hir::Constant, hir::FloatConstant>(std::move(rhs));
          switch (op) {
            case hir::Binary::Op::LT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val < r->val);
            case hir::Binary::Op::LE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val <= r->val);
            case hir::Binary::Op::GT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val > r->val);
            case hir::Binary::Op::GE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val >= r->val);

            case hir::Binary::Op::ADD:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val + r->val, true);
            case hir::Binary::Op::SUB:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val - r->val, true);

            case hir::Binary::Op::MUL:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val * r->val, true);
            case hir::Binary::Op::DIV:
              return std::make_unique<hir::FloatConstant>(
                  lhs_begin, rhs_end, l->val / r->val, true);
            case hir::Binary::Op::MOD:
              throw LocError::Create(
                  r->Begin(), "operator \% not defined on untyped float");
          }
        } break;
        case hir::Constant::Kind::CHAR: {  // char char
          auto r =
              unique_cast<hir::Constant, hir::CharConstant>(std::move(rhs));
          switch (op) {
            case hir::Binary::Op::LT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val < r->val);
            case hir::Binary::Op::LE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val <= r->val);
            case hir::Binary::Op::GT:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val > r->val);
            case hir::Binary::Op::GE:
              return std::make_unique<hir::BoolConstant>(lhs_begin, rhs_end,
                                                         l->val >= r->val);

            case hir::Binary::Op::ADD:
              return std::make_unique<hir::CharConstant>(lhs_begin, rhs_end,
                                                         l->val + r->val);
            case hir::Binary::Op::SUB:
              return std::make_unique<hir::CharConstant>(lhs_begin, rhs_end,
                                                         l->val - r->val);

            case hir::Binary::Op::MUL:
              return std::make_unique<hir::CharConstant>(lhs_begin, rhs_end,
                                                         l->val * r->val);
            case hir::Binary::Op::DIV:
              return std::make_unique<hir::CharConstant>(lhs_begin, rhs_end,
                                                         l->val / r->val);
            case hir::Binary::Op::MOD:
              return std::make_unique<hir::CharConstant>(lhs_begin, rhs_end,
                                                         l->val % r->val);
          }
        } break;
        default:
          throw LocError::Create(lhs->Begin(), "cannot binary");
      }
    } break;

    case hir::Constant::Kind::BOOL: {
      /* auto l = (hir::BoolConstant*)lhs; */
      /* switch (rhs->ConstantKind()) { */
      /*   case hir::Constant::Kind::BOOL: { */
      /*     auto r = (hir::BoolConstant*)rhs; */
      throw CompileError::Create("unimplemented bool bool");

      /* } break; */
      /* default: */
      /*   throw LocError::Create(lhs->Begin(), "cannot binary"); */
      /* } */

    } break;

    case hir::Constant::Kind::STRING: {
      /* auto lBl = (hir::BoolConstant*)lhs; */
      /* switch (rhs->ConstantKind()) { */
      /* case hir::Constant::Kind::STRING: { */
      throw CompileError::Create("unimplemented str str");

      /* } break; */
      /* default: */
      /*   throw LocError::Create(lhs->Begin(), "cannot binary"); */
      /* } */

    } break;
  }
  return nullptr;
}

std::unique_ptr<hir::Binary> Checker::CheckBinary(
    std::unique_ptr<hir::Binary> binary) {
  auto lhs_ty = binary->lhs->Ty();
  auto rhs_ty = binary->rhs->Ty();

  if (!lhs_ty->IsNumeric()) {
    throw LocError::Create(binary->lhs->Begin(), "lhs is not numeric type");
  }
  if (!rhs_ty->IsNumeric()) {
    throw LocError::Create(binary->rhs->Begin(), "rhs is not numeric type");
  }

  if (*lhs_ty == *rhs_ty) return std::move(binary);

  switch (binary->op) {
    case hir::Binary::Op::LT:
      break;
    case hir::Binary::Op::LE:
      break;
    case hir::Binary::Op::GT:
      break;
    case hir::Binary::Op::GE:
      break;

    case hir::Binary::Op::ADD:
      break;
    case hir::Binary::Op::SUB:
      break;
    case hir::Binary::Op::MUL:
      break;
    case hir::Binary::Op::DIV:
      break;
    case hir::Binary::Op::MOD:
      if (!lhs_ty->IsI32() && !lhs_ty->IsI64() && !lhs_ty->IsChar())
        throw LocError::Create(binary->lhs->Begin(),
                               "operator \% not defined on untyped float");
      if (!rhs_ty->IsI32() && !rhs_ty->IsI64() && !rhs_ty->IsChar())
        throw LocError::Create(binary->rhs->Begin(),
                               "operator \% not defined on untyped float");
      break;
  }

  // No beginsibility Constant + Constant because it should be treated in
  // MakeConstBinary. If lhs is constant, rhs should not be constant so
  // rhs is preferred.
  bool is_right_prior = binary->lhs->IsConstant();
  if (is_right_prior) {
    binary->lhs = TryExprTy(std::move(binary->lhs), rhs_ty);
  } else {
    binary->rhs = TryExprTy(std::move(binary->rhs), lhs_ty);
  }
  return std::move(binary);
}

std::unique_ptr<hir::Expr> Checker::TryConstantTy(
    std::unique_ptr<hir::Constant> cons, std::shared_ptr<Type> ty) {
  auto begin = cons->Begin();
  auto end = cons->End();
  switch (cons->ConstantKind()) {
    case hir::Constant::Kind::CHAR: {
      // char may be int or float
      auto char_const =
          unique_cast<hir::Constant, hir::CharConstant>(std::move(cons));
      switch (ty->TypeKind()) {
        case Type::CHAR:
          return std::move(char_const);
        case Type::I32:
        case Type::I64: {
          auto rune = char_const->val;
          return std::make_unique<hir::IntConstant>(begin, end, rune);
        } break;

        case Type::F32:
        case Type::F64: {
          auto rune = char_const->val;
          return std::make_unique<hir::FloatConstant>(begin, end, rune,
                                                      ty->IsF32());
        } break;

        default:
          throw LocError::Create(begin, "cannot cast char literal");
      }
    } break;

    case hir::Constant::Kind::INT: {
      auto int_const =
          unique_cast<hir::Constant, hir::IntConstant>(std::move(cons));
      switch (ty->TypeKind()) {
        case Type::CHAR: {
          if (int_const->is_32) {
            auto val = int_const->val;
            return std::make_unique<hir::CharConstant>(begin, end, val);
          }

          throw LocError::Create(begin, "overflow char");
        } break;

        case Type::I32: {
          if (int_const->is_32) return std::move(cons);

          if (int_const->val > INT32_MAX)
            throw LocError::Create(begin, "overflow int32");

          int_const->is_32 = true;
        } break;

        case Type::I64:
          break;

        case Type::F32: {
          if (int_const->is_32) {
            auto val = int_const->val;
            return std::make_unique<hir::FloatConstant>(begin, end, val, true);
          }

          throw LocError::Create(begin, "overflow f32");
        } break;

        case Type::F64: {
          auto rune = int_const->val;
          return std::make_unique<hir::FloatConstant>(begin, end, rune, false);
        } break;

        default:
          throw LocError::Create(begin, "can't cast");
      }
    } break;

    case hir::Constant::Kind::BOOL: {
      if (!ty->IsBool())
        throw LocError::Create(cons->Begin(), "can't cast bool");
      return std::move(cons);
    } break;

    case hir::Constant::Kind::FLOAT: {
      auto float_const =
          unique_cast<hir::Constant, hir::FloatConstant>(std::move(cons));
      switch (ty->TypeKind()) {
        case Type::F32: {
          if (float_const->is_32) {
            return std::move(float_const);
          }
          throw LocError::Create(begin, "overflow f32");
        } break;

        case Type::F64: {
          float_const->is_32 = false;
          return std::move(float_const);
        } break;

        default:
          throw LocError::Create(begin, "can't cast");
      }

    } break;

    case hir::Constant::Kind::STRING: {
      if (!ty->IsString())
        throw LocError::Create(cons->Begin(), "can't cast string");
      return std::move(cons);
    } break;
  }
  UNREACHABLE
}

// check exp's type and try to set type `ty`
std::unique_ptr<hir::Expr> Checker::TryExprTy(std::unique_ptr<hir::Expr> expr,
                                              std::shared_ptr<Type> ty) {
  switch (expr->ExprKind()) {
    case hir::Expr::Kind::VALUE: {
      auto value = (hir::Value*)expr.get();
      switch (value->ValueKind()) {
        case hir::Value::Kind::VARIABLE: {
          // Variables can't be casted implicitly
          auto var = (hir::Variable*)value;
          if (*var->decl->type != *ty) {
            throw LocError::Create(expr->Begin(), "unmatched variable type");
          }
          return std::move(expr);
        } break;
        case hir::Value::Kind::CONSTANT: {
          // Constants can be casted implicitly
          auto cons = unique_cast<hir::Expr, hir::Constant>(std::move(expr));
          return TryConstantTy(std::move(cons), ty);
        } break;
      }
    } break;

    case hir::Expr::Kind::BINARY: {
      auto binary = unique_cast<hir::Expr, hir::Binary>(std::move(expr));
      if (*binary->Ty() == *ty) return std::move(binary);
      binary->lhs = TryExprTy(std::move(binary->lhs), ty);
      binary->rhs = TryExprTy(std::move(binary->rhs), ty);
      if (*binary->Ty() != *ty) {
        throw LocError::Create(binary->Begin(), "unmatched binary type");
      }
      return std::move(binary);
    } break;

    default: {
      if (*expr->Ty() != *ty) {
        throw LocError::Create(expr->Begin(), "unmatched exp type");
      }
    } break;
  }
  UNREACHABLE
}

std::unique_ptr<hir::Constant> Checker::MakeLit(std::unique_ptr<ast::Lit> lit) {
  auto begin = lit->Begin();
  auto end = lit->End();
  switch (lit->LitKind()) {
    case ast::Lit::Kind::INT:
      return ParseInt(std::move(lit));

    case ast::Lit::Kind::FLOAT: {
      auto val = ParseFloat(std::move(lit));
      return std::make_unique<hir::FloatConstant>(begin, end, val, true);
    } break;
    case ast::Lit::Kind::BOOL: {
      return std::make_unique<hir::BoolConstant>(begin, end,
                                                 lit->val == "true");
    } break;
    case ast::Lit::Kind::CHAR: {
      std::stringstream ss(lit->val);
      rune r;
      ss >> r;
      return std::make_unique<hir::CharConstant>(begin, end, r);
    } break;
    case ast::Lit::Kind::STRING: {
      return std::make_unique<hir::StringConstant>(begin, end, lit->val);
    } break;
  }
}

std::unique_ptr<hir::IntConstant> Checker::ParseInt(
    std::unique_ptr<ast::Lit> lit) {
  try {
    // TODO: parse int
    long long n = stoll(lit->val);
    if (n <= INT64_MAX) {
      return std::make_unique<hir::IntConstant>(lit->Begin(), lit->End(), n);
    } else {
      throw LocError::Create(lit->Begin(), "overflow int64");
    }
  } catch (std::out_of_range e) {
    throw LocError::Create(lit->Begin(), "out of range");
  } catch (std::invalid_argument e) {
    throw LocError::Create(lit->Begin(), "invalid or unimplemented");
  }
}

double Checker::ParseFloat(std::unique_ptr<ast::Lit> lit) {
  try {
    // TODO:parse float
    double n = stod(lit->val);
    return n;
  } catch (std::out_of_range e) {
    throw LocError::Create(lit->Begin(), "out of range");
  } catch (std::invalid_argument e) {
    throw LocError::Create(lit->Begin(), "invalid or unimplemented");
  }
}

std::shared_ptr<Decl> Checker::InsertFnDecl(
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

void Checker::OpenScope() {
  current_scope_ = std::make_shared<Scope>(current_scope_);
}

void Checker::CloseScope() {
  assert(!current_scope_->IsTop());
  current_scope_ = current_scope_->GetParent();
}

bool Checker::CanDecl(std::string name) {
  return current_scope_->FindDecl(name) == nullptr;
}

void Checker::DebugScope() {
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

std::shared_ptr<Decl> Checker::LookupDecl(std::string name) {
  auto scope = current_scope_;
  while (scope) {
    auto def = scope->FindDecl(name);
    if (def) return def;
    scope = scope->GetParent();
  }
  return nullptr;
}

std::shared_ptr<Type> Checker::LookupType(std::string name) {
  auto scope = current_scope_;
  while (scope) {
    auto ty = scope->FindType(name);
    if (ty) return ty;
    scope = scope->GetParent();
  }
  return nullptr;
}

}  // namespace felis
