#include "check/check.h"

#include <assert.h>

#include "error/error.h"
#include "macro.h"
#include "ptr.h"

namespace felis {

void Checker::SetupBuiltin() {
  assert(currentScope_->IsTop());
  // insert basic types into global scope
  currentScope_->InsertType("void", std::make_shared<Type>(Type::Kind::VOID));
  currentScope_->InsertType("i32", std::make_shared<Type>(Type::Kind::I32));
  currentScope_->InsertType("i64", std::make_shared<Type>(Type::Kind::I64));
  currentScope_->InsertType("f32", std::make_shared<Type>(Type::Kind::F32));
  currentScope_->InsertType("f64", std::make_shared<Type>(Type::Kind::F64));
  currentScope_->InsertType("bool", std::make_shared<Type>(Type::Kind::BOOL));
  currentScope_->InsertType("char", std::make_shared<Type>(Type::Kind::CHAR));
  currentScope_->InsertType("string",
                            std::make_shared<Type>(Type::Kind::STRING));
}

std::unique_ptr<hir::File> Checker::Check(std::unique_ptr<ast::File> file) {
  auto hirFile = std::make_unique<hir::File>();
  for (auto& ext : file->externs) {
    auto decl = InsertFnDecl(true, ext->proto);
    hirFile->externs.emplace_back(new hir::Extern(decl));
  }
  for (auto& fn : file->fnDecls) {
    auto decl = InsertFnDecl(false, fn->proto);
    hirFile->fnDecls.emplace_back(new hir::FnDecl(decl));
  }

  int i = 0;
  while (!file->fnDecls.empty()) {
    auto fn = std::move(file->fnDecls.front());
    file->fnDecls.pop_front();
    hirFile->fnDecls[i]->block =
        CheckFnDecl(std::move(fn), hirFile->fnDecls[i]);
    i++;
  }
  return std::move(hirFile);
}

std::unique_ptr<hir::Block> Checker::CheckFnDecl(
    std::unique_ptr<ast::FnDecl> fnDecl,
    std::unique_ptr<hir::FnDecl>& hirDecl) {
  currentFunc_ = hirDecl->decl;
  OpenScope();
  for (auto& arg : fnDecl->proto->args) {
    // arg-name duplication is already checked in parser
    auto argDecl = std::make_shared<Decl>(
        arg->name->val, LookupType(arg->ty->val), Decl::Kind::ARG);
    currentScope_->InsertDecl(arg->name->val, argDecl);
    hirDecl->args.push_back(argDecl);
  }
  auto block = CheckBlock(std::move(fnDecl->block), true);
  CloseScope();

  if (!block->IsTerminating()) {
    if (currentFunc_->AsFuncType()->ret->IsVoid()) {
      block->stmts.push_back(std::make_unique<hir::RetStmt>());

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
    std::unique_ptr<ast::RetStmt> retStmt) {
  auto funcType = currentFunc_->AsFuncType();
  if (retStmt->expr) {
    auto expr = MakeExpr(std::move(retStmt->expr));
    if (*expr->Ty() != *funcType->ret)
      expr = TryExprTy(std::move(expr), funcType->ret);
    /* expr->Debug(); */
    return std::make_unique<hir::RetStmt>(std::move(expr));
  } else {
    // empty return
    if (!funcType->ret->IsVoid()) {
      throw CompileError::CreatePos(retStmt->GetPos(), "func type is not void");
    }
    return std::make_unique<hir::RetStmt>();
  }
}

std::unique_ptr<hir::VarDeclStmt> Checker::CheckVarDeclStmt(
    std::unique_ptr<ast::VarDeclStmt> declStmt) {
  std::string name = declStmt->name->val;
  if (!CanDecl(name)) {
    throw CompileError::CreatePos(declStmt->GetPos(), "redeclared var %s",
                                  name.c_str());
  }
  auto exp = MakeExpr(std::move(declStmt->expr));
  auto decl = std::make_shared<Decl>(
      name, exp->Ty(), declStmt->isLet ? Decl::Kind::LET : Decl::Kind::VAR);
  currentScope_->InsertDecl(name, decl);
  /* decl->Debug(); */
  /* DebugScope(); */
  return std::make_unique<hir::VarDeclStmt>(decl, std::move(exp));
}

std::unique_ptr<hir::AssignStmt> Checker::CheckAssignStmt(
    std::unique_ptr<ast::AssignStmt> assignStmt) {
  auto name = assignStmt->name->val;
  auto decl = LookupDecl(name);
  if (!decl) {
    throw CompileError::CreatePos(assignStmt->GetPos(), "undeclared var %s",
                                  name.c_str());
  }
  if (decl->IsFunc()) {
    throw CompileError::CreatePos(assignStmt->GetPos(),
                                  "%s is declared as function", name.c_str());
  }
  if (!decl->IsAssignable()) {
    throw CompileError::CreatePos(assignStmt->GetPos(),
                                  "%s is declared as mutable variable",
                                  name.c_str());
  }
  auto expr = MakeExpr(std::move(assignStmt->expr));
  expr = TryExprTy(std::move(expr), decl->type);
  return std::make_unique<hir::AssignStmt>(decl, std::move(expr));
}

std::unique_ptr<hir::IfStmt> Checker::CheckIfStmt(
    std::unique_ptr<ast::IfStmt> ifStmt) {
  auto cond = MakeExpr(std::move(ifStmt->cond));
  if (!cond->Ty()->IsBool()) {
    throw CompileError::CreatePos(ifStmt->cond->GetPos(), "non bool if cond");
  }
  auto block = CheckBlock(std::move(ifStmt->block));

  if (ifStmt->els) {
    if (ifStmt->els->StmtKind() == ast::Stmt::Kind::IF) {
      auto els = CheckIfStmt(
          unique_cast<ast::Stmt, ast::IfStmt>(std::move(ifStmt->els)));
      return std::make_unique<hir::IfStmt>(std::move(cond), std::move(block),
                                           std::move(els));
    } else if (ifStmt->els->StmtKind() == ast::Stmt::Kind::BLOCK) {
      auto els = CheckBlock(
          unique_cast<ast::Stmt, ast::Block>(std::move(ifStmt->els)));
      return std::make_unique<hir::IfStmt>(std::move(cond), std::move(block),
                                           std::move(els));
    }
  }
  return std::make_unique<hir::IfStmt>(std::move(cond), std::move(block));
}

std::unique_ptr<hir::Block> Checker::CheckBlock(
    std::unique_ptr<ast::Block> block, bool isFnBody) {
  if (!isFnBody) OpenScope();

  auto b = std::make_unique<hir::Block>();
  while (!block->stmts.empty()) {
    auto stmtAst = std::move(block->stmts.front());
    block->stmts.pop_front();

    bool isLast = block->stmts.empty();
    auto stmt = CheckStmt(std::move(stmtAst));
    bool isTerminating = stmt->IsTerminating();

    if (isTerminating && !isLast) {
      auto& nextStmt = block->stmts.front();
      throw CompileError::CreatePos(nextStmt->GetPos(), "unreachable code");
    }

    b->stmts.push_back(std::move(stmt));
  }

  if (!isFnBody) CloseScope();
  return std::move(b);
}

std::unique_ptr<hir::Expr> Checker::MakeExpr(std::unique_ptr<ast::Expr> expr) {
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT: {
      return MakeLit(unique_cast<ast::Expr, ast::Lit>(std::move(expr)));
    }
    case ast::Expr::Kind::CALL: {
      auto callExpr = unique_cast<ast::Expr, ast::CallExpr>(std::move(expr));

      auto decl = LookupDecl(callExpr->ident->val);
      if (decl == nullptr) {
        throw CompileError::CreatePos(callExpr->GetPos(),
                                      "undefined function %s",
                                      callExpr->ident->val.c_str());
      }
      if (!decl->IsFunc()) {
        throw CompileError::CreatePos(callExpr->GetPos(),
                                      "%s is not declared as function",
                                      callExpr->ident->val.c_str());
      }
      auto fnType = (FuncType*)decl->type.get();
      if (fnType->args.size() != callExpr->args.size()) {
        throw CompileError::CreatePos(callExpr->GetPos(),
                                      "args count doesn't match");
      }
      auto call = std::make_unique<hir::Call>(callExpr->GetPos());
      call->decl = decl;

      int i = 0;
      while (!callExpr->args.empty()) {
        auto arg = std::move(callExpr->args.front());
        callExpr->args.pop_front();

        auto exp = MakeExpr(std::move(arg));
        auto ty = fnType->args[i];
        exp = TryExprTy(std::move(exp), ty);
        call->args.push_back(std::move(exp));
        i++;
      }
      return call;
    } break;

    case ast::Expr::Kind::IDENT: {
      auto ident = unique_cast<ast::Expr, ast::Ident>(std::move(expr));
      auto pos = ident->GetPos();
      auto decl = LookupDecl(ident->val);
      if (decl == nullptr) {
        throw CompileError::CreatePos(pos, "undefined function %s",
                                      ident->val.c_str());
      }
      if (decl->IsFunc()) {
        throw CompileError::CreatePos(pos, "%s is not declared as variable",
                                      ident->val.c_str());
      }
      auto value = std::make_unique<hir::Variable>(pos);
      value->decl = decl;
      return value;

    } break;

    case ast::Expr::Kind::UNARY: {
      auto unaryExpr = unique_cast<ast::Expr, ast::UnaryExpr>(std::move(expr));
      auto e = MakeExpr(std::move(unaryExpr->expr));
      if (e->IsConstant()) {
        return MakeConstUnary(
            unique_cast<hir::Expr, hir::Constant>(std::move(e)),
            unaryExpr->unOp);
      }
      return std::make_unique<hir::Unary>(unaryExpr->GetPos(), unaryExpr->unOp,
                                          std::move(e));
    } break;

    case ast::Expr::Kind::BINARY: {
      auto binary = unique_cast<ast::Expr, ast::BinaryExpr>(std::move(expr));
      auto pos = binary->GetPos();
      auto lhs = MakeExpr(std::move(binary->lhs));
      auto rhs = MakeExpr(std::move(binary->rhs));
      if (lhs->IsConstant() && rhs->IsConstant()) {
        return MakeConstBinary(
            unique_cast<hir::Expr, hir::Constant>(std::move(lhs)),
            unique_cast<hir::Expr, hir::Constant>(std::move(rhs)), binary->op);
      }
      return CheckBinary(std::make_unique<hir::Binary>(
          pos, binary->op, std::move(lhs), std::move(rhs)));
    } break;
    default:
      return nullptr;
  }
}

std::unique_ptr<hir::Constant> Checker::MakeConstUnary(
    std::unique_ptr<hir::Constant> cons, ast::UnOp op) {
  switch (op) {
    case ast::UnOp::NOT:
      if (cons->Ty()->IsBool()) {
        auto b = unique_cast<hir::Constant, hir::BoolConstant>(std::move(cons));
        b->val = !b->val;
        return std::move(b);
      } else {
        throw CompileError::CreatePos(cons->pos, "non bool type");
      }
    case ast::UnOp::NEG:
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
        throw CompileError::CreatePos(cons->pos, "non numeric type");
      }
  }
}

std::unique_ptr<hir::Constant> Checker::MakeConstBinary(
    std::unique_ptr<hir::Constant> lhs, std::unique_ptr<hir::Constant> rhs,
    ast::BinOp op) {
  auto lhsPos = lhs->pos;
  switch (lhs->ConstantKind()) {
    case hir::Constant::Kind::INT: {
      auto l = unique_cast<hir::Constant, hir::IntConstant>(std::move(lhs));
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // int int
          auto r = unique_cast<hir::Constant, hir::IntConstant>(std::move(rhs));
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val / r->val);
            case ast::BinOp::MOD:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val % r->val);
          }
        } break;

        case hir::Constant::Kind::FLOAT: {  // int float
          auto r =
              unique_cast<hir::Constant, hir::FloatConstant>(std::move(rhs));
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val + r->val, true);
            case ast::BinOp::SUB:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val - r->val, true);

            case ast::BinOp::MUL:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val * r->val, true);
            case ast::BinOp::DIV:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val / r->val, true);
            case ast::BinOp::MOD:
              throw CompileError::CreatePos(
                  r->pos, "operator \% not defined on untyped float");
          }
        } break;

        case hir::Constant::Kind::CHAR: {  // int char
          auto r =
              unique_cast<hir::Constant, hir::CharConstant>(std::move(rhs));
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val / r->val);
            case ast::BinOp::MOD:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val % r->val);
          }
        } break;
        default:
          throw CompileError::CreatePos(lhs->pos, "cannot binary");
      }
    } break;

    case hir::Constant::Kind::FLOAT: {
      auto l = unique_cast<hir::Constant, hir::FloatConstant>(std::move(lhs));
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // float int
          auto r = unique_cast<hir::Constant, hir::IntConstant>(std::move(rhs));
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val + r->val, true);
            case ast::BinOp::SUB:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val - r->val, true);

            case ast::BinOp::MUL:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val * r->val, true);
            case ast::BinOp::DIV:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val / r->val, true);
            case ast::BinOp::MOD:
              throw CompileError::CreatePos(
                  l->pos, "operator \% not defined on untyped float");
          }
        } break;
        case hir::Constant::Kind::FLOAT: {  // float float
          auto r =
              unique_cast<hir::Constant, hir::FloatConstant>(std::move(rhs));
          bool is32 = l->is32 && r->is32;
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val + r->val, is32);
            case ast::BinOp::SUB:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val - r->val, is32);

            case ast::BinOp::MUL:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val * r->val, is32);
            case ast::BinOp::DIV:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val / r->val, is32);
            case ast::BinOp::MOD:
              throw CompileError::CreatePos(
                  l->pos, "operator \% not defined on untyped float");
          }
        } break;

        case hir::Constant::Kind::CHAR: {  // float char
          auto r =
              unique_cast<hir::Constant, hir::CharConstant>(std::move(rhs));
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val + r->val, true);
            case ast::BinOp::SUB:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val - r->val, true);

            case ast::BinOp::MUL:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val * r->val, true);
            case ast::BinOp::DIV:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val / r->val, true);
            case ast::BinOp::MOD:
              throw CompileError::CreatePos(
                  l->pos, "operator \% not defined on untyped float");
          }
        } break;
        default:
          throw CompileError::CreatePos(lhs->pos, "cannot binary");
      }
    } break;

    case hir::Constant::Kind::CHAR: {
      auto l = unique_cast<hir::Constant, hir::CharConstant>(std::move(lhs));
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // char int
          auto r = unique_cast<hir::Constant, hir::IntConstant>(std::move(rhs));
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val / r->val);
            case ast::BinOp::MOD:
              return std::make_unique<hir::IntConstant>(lhsPos,
                                                        l->val % r->val);
          }
        } break;
        case hir::Constant::Kind::FLOAT: {  // char float
          auto r =
              unique_cast<hir::Constant, hir::FloatConstant>(std::move(rhs));
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val + r->val, true);
            case ast::BinOp::SUB:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val - r->val, true);

            case ast::BinOp::MUL:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val * r->val, true);
            case ast::BinOp::DIV:
              return std::make_unique<hir::FloatConstant>(
                  lhsPos, l->val / r->val, true);
            case ast::BinOp::MOD:
              throw CompileError::CreatePos(
                  r->pos, "operator \% not defined on untyped float");
          }
        } break;
        case hir::Constant::Kind::CHAR: {  // char char
          auto r =
              unique_cast<hir::Constant, hir::CharConstant>(std::move(rhs));
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhsPos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::CharConstant>(lhsPos,
                                                         l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::CharConstant>(lhsPos,
                                                         l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::CharConstant>(lhsPos,
                                                         l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::CharConstant>(lhsPos,
                                                         l->val / r->val);
            case ast::BinOp::MOD:
              return std::make_unique<hir::CharConstant>(lhsPos,
                                                         l->val % r->val);
          }
        } break;
        default:
          throw CompileError::CreatePos(lhs->pos, "cannot binary");
      }
    } break;

    case hir::Constant::Kind::BOOL: {
      /* auto l = (hir::BoolConstant*)lhs; */
      /* switch (rhs->ConstantKind()) { */
      /*   case hir::Constant::Kind::BOOL: { */
      /*     auto r = (hir::BoolConstant*)rhs; */
      /*     throw CompileError::Create("unimplemented bool bool"); */
      /*   } break; */
      /*   default: */
      throw CompileError::CreatePos(lhs->pos, "cannot binary");
      /* } */
    } break;

    case hir::Constant::Kind::STRING: {
      /* auto lBl = (hir::BoolConstant*)lhs; */
      /* switch (rhs->ConstantKind()) { */
      /*   case hir::Constant::Kind::STRING: { */
      /*     throw CompileError::Create("unimplemented str str"); */
      /*   } break; */
      /*   default: */
      throw CompileError::CreatePos(lhs->pos, "cannot binary");
      /* } */
    } break;
  }
  return nullptr;
}

std::unique_ptr<hir::Binary> Checker::CheckBinary(
    std::unique_ptr<hir::Binary> binary) {
  auto lhsTy = binary->lhs->Ty();
  auto rhsTy = binary->rhs->Ty();

  if (!lhsTy->IsNumeric()) {
    throw CompileError::CreatePos(binary->lhs->pos, "lhs is not numeric type");
  }
  if (!rhsTy->IsNumeric()) {
    throw CompileError::CreatePos(binary->rhs->pos, "rhs is not numeric type");
  }

  if (*lhsTy == *rhsTy) return std::move(binary);

  switch (binary->binOp) {
    case ast::BinOp::LT:
      break;
    case ast::BinOp::LE:
      break;
    case ast::BinOp::GT:
      break;
    case ast::BinOp::GE:
      break;

    case ast::BinOp::ADD:
      break;
    case ast::BinOp::SUB:
      break;
    case ast::BinOp::MUL:
      break;
    case ast::BinOp::DIV:
      break;
    case ast::BinOp::MOD:
      if (!lhsTy->IsI32() && !lhsTy->IsI64() && !lhsTy->IsChar())
        throw CompileError::CreatePos(
            binary->lhs->pos, "operator \% not defined on untyped float");
      if (!rhsTy->IsI32() && !rhsTy->IsI64() && !rhsTy->IsChar())
        throw CompileError::CreatePos(
            binary->rhs->pos, "operator \% not defined on untyped float");
      break;
  }

  // No possibility Constant + Constant because it should be treated in
  // MakeConstBinary. If lhs is constant, rhs should not be constant so
  // rhs is preferred.
  bool isRightPrior = binary->lhs->IsConstant();
  if (isRightPrior) {
    binary->lhs = TryExprTy(std::move(binary->lhs), rhsTy);
  } else {
    binary->rhs = TryExprTy(std::move(binary->rhs), lhsTy);
  }
  return std::move(binary);
}

std::unique_ptr<hir::Expr> Checker::TryConstantTy(
    std::unique_ptr<hir::Constant> cons, std::shared_ptr<Type> ty) {
  switch (cons->ConstantKind()) {
    case hir::Constant::Kind::CHAR: {
      // char may be int or float
      auto charConst =
          unique_cast<hir::Constant, hir::CharConstant>(std::move(cons));
      auto pos = charConst->pos;
      switch (ty->TypeKind()) {
        case Type::CHAR:
          return std::move(charConst);
        case Type::I32:
        case Type::I64: {
          auto rune = charConst->val;
          return std::make_unique<hir::IntConstant>(pos, rune);
        } break;

        case Type::F32:
        case Type::F64: {
          auto rune = charConst->val;
          return std::make_unique<hir::FloatConstant>(pos, rune, ty->IsF32());
        } break;

        default:
          throw CompileError::CreatePos(pos, "cannot cast char literal");
      }
    } break;

    case hir::Constant::Kind::INT: {
      auto intConst =
          unique_cast<hir::Constant, hir::IntConstant>(std::move(cons));
      auto pos = intConst->pos;
      switch (ty->TypeKind()) {
        case Type::CHAR: {
          if (intConst->is32) {
            auto val = intConst->val;
            return std::make_unique<hir::CharConstant>(pos, val);
          }

          throw CompileError::CreatePos(pos, "overflow char");
        } break;

        case Type::I32: {
          if (intConst->is32) return std::move(cons);

          if (intConst->val > INT32_MAX)
            throw CompileError::CreatePos(pos, "overflow int32");

          intConst->is32 = true;
        } break;

        case Type::I64:
          break;

        case Type::F32: {
          if (intConst->is32) {
            auto val = intConst->val;
            return std::make_unique<hir::FloatConstant>(pos, val, true);
          }

          throw CompileError::CreatePos(pos, "overflow f32");
        } break;

        case Type::F64: {
          auto rune = intConst->val;
          return std::make_unique<hir::FloatConstant>(pos, rune, false);
        } break;

        default:
          throw CompileError::CreatePos(pos, "can't cast");
      }
    } break;

    case hir::Constant::Kind::BOOL: {
      if (!ty->IsBool())
        throw CompileError::CreatePos(cons->pos, "can't cast bool");
      return std::move(cons);
    } break;

    case hir::Constant::Kind::FLOAT: {
      auto floatConst =
          unique_cast<hir::Constant, hir::FloatConstant>(std::move(cons));
      auto pos = floatConst->pos;
      switch (ty->TypeKind()) {
        case Type::F32: {
          if (floatConst->is32) {
            return std::move(floatConst);
          }
          throw CompileError::CreatePos(pos, "overflow f32");
        } break;

        case Type::F64: {
          floatConst->is32 = false;
          return std::move(floatConst);
        } break;

        default:
          throw CompileError::CreatePos(pos, "can't cast");
      }

    } break;

    case hir::Constant::Kind::STRING: {
      if (!ty->IsString())
        throw CompileError::CreatePos(cons->pos, "can't cast string");
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
            throw CompileError::CreatePos(expr->pos, "unmatched variable type");
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
        throw CompileError::CreatePos(binary->pos, "unmatched binary type");
      }
      return std::move(binary);
    } break;

    default: {
      if (*expr->Ty() != *ty) {
        throw CompileError::CreatePos(expr->pos, "unmatched exp type");
      }
    } break;
  }
  UNREACHABLE
}

std::unique_ptr<hir::Constant> Checker::MakeLit(std::unique_ptr<ast::Lit> lit) {
  switch (lit->LitKind()) {
    case ast::Lit::Kind::INT:
      return ParseInt(std::move(lit));

    case ast::Lit::Kind::FLOAT: {
      auto pos = lit->GetPos();
      auto val = ParseFloat(std::move(lit));
      return std::make_unique<hir::FloatConstant>(pos, val, true);
    } break;
    case ast::Lit::Kind::BOOL: {
      return std::make_unique<hir::BoolConstant>(lit->GetPos(),
                                                 lit->val == "true");
    } break;
    case ast::Lit::Kind::CHAR: {
      std::stringstream ss(lit->val);
      rune r = consumeRune(ss);
      return std::make_unique<hir::CharConstant>(lit->GetPos(), r);
    } break;
    case ast::Lit::Kind::STRING: {
      return std::make_unique<hir::StringConstant>(lit->GetPos(), lit->val);
    } break;
  }
}

std::unique_ptr<hir::IntConstant> Checker::ParseInt(
    std::unique_ptr<ast::Lit> lit) {
  try {
    // TODO: parse int
    long long n = stoll(lit->val);
    if (n <= INT64_MAX) {
      return std::make_unique<hir::IntConstant>(lit->GetPos(), n);
    } else {
      throw CompileError::CreatePos(lit->GetPos(), "overflow int64");
    }
  } catch (std::out_of_range e) {
    throw CompileError::CreatePos(lit->GetPos(), "out of range");
  } catch (std::invalid_argument e) {
    throw CompileError::CreatePos(lit->GetPos(), "invalid or unimplemented");
  }
}

double Checker::ParseFloat(std::unique_ptr<ast::Lit> lit) {
  try {
    // TODO:parse float
    double n = stod(lit->val);
    return n;
  } catch (std::out_of_range e) {
    throw CompileError::CreatePos(lit->GetPos(), "out of range");
  } catch (std::invalid_argument e) {
    throw CompileError::CreatePos(lit->GetPos(), "invalid or unimplemented");
  }
}

std::shared_ptr<Decl> Checker::InsertFnDecl(
    bool isExt, const std::unique_ptr<ast::FnProto>& proto) {
  if (!CanDecl(proto->name->val)) {
    throw CompileError::CreatePos(proto->name->GetPos(),
                                  "redeclared function %s",
                                  proto->name->val.c_str());
  }

  std::vector<std::shared_ptr<Type>> args;
  for (auto& arg : proto->args) {
    auto ty = LookupType(arg->ty->val);
    if (!ty) {
      throw CompileError::CreatePos(arg->ty->GetPos(), "unknown arg type %s",
                                    arg->ty->val.c_str());
    }
    args.push_back(ty);
  }
  std::shared_ptr<Type> retTy;
  if (proto->ret) {
    retTy = LookupType(proto->ret->val);
    if (!retTy) {
      throw CompileError::CreatePos(proto->ret->GetPos(), "unknown ret type %s",
                                    proto->ret->val.c_str());
    }
  } else {
    retTy = std::make_shared<Type>(Type::Kind::VOID);
  }
  auto fnType = std::make_shared<FuncType>(std::move(args), std::move(retTy));
  Decl::Kind kind = isExt ? Decl::Kind::EXT : Decl::Kind::FN;
  auto decl = std::make_shared<Decl>(proto->name->val, fnType, kind);
  currentScope_->InsertDecl(proto->name->val, decl);
  return decl;
}

void Checker::OpenScope() {
  currentScope_ = std::make_shared<Scope>(currentScope_);
}

void Checker::CloseScope() {
  assert(!currentScope_->IsTop());
  currentScope_ = currentScope_->GetParent();
}

bool Checker::CanDecl(std::string name) {
  return currentScope_->FindDecl(name) == nullptr;
}

void Checker::DebugScope() {
  std::cout << "----------------" << std::endl;
  auto scope = currentScope_;
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
  auto scope = currentScope_;
  while (scope) {
    auto def = scope->FindDecl(name);
    if (def) return def;
    scope = scope->GetParent();
  }
  return nullptr;
}

std::shared_ptr<Type> Checker::LookupType(std::string name) {
  auto scope = currentScope_;
  while (scope) {
    auto ty = scope->FindType(name);
    if (ty) return ty;
    scope = scope->GetParent();
  }
  return nullptr;
}

}  // namespace felis
