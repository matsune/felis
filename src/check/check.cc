#include "check/check.h"

#include <assert.h>

#include "error/error.h"

namespace felis {

void Checker::SetupBuiltin() {
  assert(currentScope_->IsTop());
  // insert basic types into global scope
  currentScope_->InsertType("void", std::make_shared<Type>(Type::Kind::VOID));
  currentScope_->InsertType("i32", std::make_shared<Type>(Type::Kind::I32));
  currentScope_->InsertType("bool", std::make_shared<Type>(Type::Kind::BOOL));
  currentScope_->InsertType("char", std::make_shared<Type>(Type::Kind::CHAR));
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

  for (int i = 0; i < file->fnDecls.size(); i++) {
    auto decl = hirFile->fnDecls[i]->decl;
    decl->Debug();
    currentFunc_ = decl;
    auto& fn = file->fnDecls[i];
    hirFile->fnDecls[i]->stmts = CheckFnDecl(fn);
  }
  return std::move(hirFile);
}

std::vector<std::unique_ptr<hir::Stmt>> Checker::CheckFnDecl(
    std::unique_ptr<ast::FnDecl>& fnDecl) {
  OpenScope();
  for (auto& arg : fnDecl->proto->args) {
    // arg-name duplication is already checked in parser
    auto argDecl = std::make_shared<Decl>(
        arg->name->val, LookupType(arg->ty->val), Decl::Kind::ARG);
    currentScope_->InsertDecl(arg->name->val, argDecl);
  }
  std::vector<std::unique_ptr<hir::Stmt>> stmts;
  for (auto& stmt : fnDecl->block->stmts) {
    stmts.push_back(std::move(CheckStmt(stmt)));
  }
  CloseScope();
  return stmts;
}

std::unique_ptr<hir::Stmt> Checker::CheckStmt(
    std::unique_ptr<ast::Stmt>& stmt) {
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      // only type check
      return MakeExpr((ast::Expr*)stmt.get());
    case ast::Stmt::Kind::RET:
      return CheckRetStmt((ast::RetStmt*)stmt.get());
    case ast::Stmt::Kind::VAR_DECL:
      return CheckVarDeclStmt((ast::VarDeclStmt*)stmt.get());
    case ast::Stmt::Kind::ASSIGN:
      return CheckAssignStmt((ast::AssignStmt*)stmt.get());
    case ast::Stmt::Kind::IF:
      return CheckIfStmt((ast::IfStmt*)stmt.get());
    case ast::Stmt::Kind::BLOCK:
      return CheckBlock((ast::Block*)stmt.get());
  }
}

std::unique_ptr<hir::RetStmt> Checker::CheckRetStmt(ast::RetStmt* retStmt) {
  auto funcType = currentFunc_->AsFuncType();
  if (retStmt->expr) {
    auto expr = MakeExpr(retStmt->expr.get());
    if (*expr->Ty() != *funcType->ret) TryExpTy(expr.get(), funcType->ret);
    expr->Debug();
    return std::make_unique<hir::RetStmt>(std::move(expr));
  } else {
    // empty return
    if (!funcType->ret->IsVoid()) {
      throw CompileError::CreatePos(retStmt->GetPos(), "func type is not void");
    }
    std::cout << " void" << std::endl;
    return std::make_unique<hir::RetStmt>();
  }
}

std::unique_ptr<hir::VarDeclStmt> Checker::CheckVarDeclStmt(
    ast::VarDeclStmt* declStmt) {
  std::cout << "varDecl ";
  std::string name = declStmt->name->val;
  if (!CanDecl(name)) {
    throw CompileError::CreatePos(declStmt->GetPos(), "redeclared var %s",
                                  name.c_str());
  }
  auto exp = MakeExpr(declStmt->expr.get());
  auto decl = std::make_shared<Decl>(
      name, exp->Ty(), declStmt->isLet ? Decl::Kind::LET : Decl::Kind::VAR);
  currentScope_->InsertDecl(name, decl);
  decl->Debug();
  /* DebugScope(); */
  return std::make_unique<hir::VarDeclStmt>(decl, std::move(exp));
}

std::unique_ptr<hir::AssignStmt> Checker::CheckAssignStmt(
    ast::AssignStmt* assignStmt) {
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
  auto expr = MakeExpr(assignStmt->expr.get());
  if (*decl->type != *expr->Ty()) {
    throw CompileError::CreatePos(assignStmt->GetPos(),
                                  "assigned expr type doesn't match");
  }
  return std::make_unique<hir::AssignStmt>(decl, std::move(expr));
}

std::unique_ptr<hir::IfStmt> Checker::CheckIfStmt(ast::IfStmt* ifStmt) {
  auto cond = ifStmt->cond.get();
  auto condExpr = MakeExpr(cond);
  if (!condExpr->Ty()->IsBool()) {
    throw CompileError::CreatePos(ifStmt->cond->GetPos(), "non bool if cond");
  }
  auto block = ifStmt->block.get();
  auto blockStmt = CheckBlock(block);

  if (ifStmt->els) {
    if (ifStmt->els->StmtKind() == ast::Stmt::Kind::IF) {
      auto elsStmt = (ast::IfStmt*)ifStmt->els.get();
      auto els = CheckIfStmt(elsStmt);
      return std::make_unique<hir::IfStmt>(
          std::move(condExpr), std::move(blockStmt), std::move(els));
    } else if (ifStmt->els->StmtKind() == ast::Stmt::Kind::BLOCK) {
      auto elsBlock = (ast::Block*)ifStmt->els.get();
      auto els = CheckBlock(elsBlock);
      return std::make_unique<hir::IfStmt>(
          std::move(condExpr), std::move(blockStmt), std::move(els));
    }
  }
  return std::make_unique<hir::IfStmt>(std::move(condExpr),
                                       std::move(blockStmt));
}

std::unique_ptr<hir::Block> Checker::CheckBlock(ast::Block* block) {
  OpenScope();
  auto b = std::make_unique<hir::Block>();
  for (auto& stmt : block->stmts) {
    b->stmts.push_back(CheckStmt(stmt));
  }
  CloseScope();
  return std::move(b);
}

std::unique_ptr<hir::Expr> Checker::MakeExpr(ast::Expr* expr) {
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT: {
      auto lit = (ast::Lit*)expr;
      return MakeLit(lit);
    } break;

    case ast::Expr::Kind::CALL: {
      auto callExpr = (ast::CallExpr*)expr;

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
      for (int i = 0; i < callExpr->args.size(); i++) {
        auto& arg = callExpr->args[i];
        auto exp = MakeExpr(arg.get());
        auto ty = fnType->args[i];
        TryExpTy(exp.get(), ty);
        call->argExprs.push_back(std::move(exp));
      }
      return std::move(call);
    } break;

    case ast::Expr::Kind::IDENT: {
      auto ident = (ast::Ident*)expr;
      auto decl = LookupDecl(ident->val);
      if (decl == nullptr) {
        throw CompileError::CreatePos(ident->GetPos(), "undefined function %s",
                                      ident->val.c_str());
      }
      if (decl->IsFunc()) {
        throw CompileError::CreatePos(ident->GetPos(),
                                      "%s is not declared as variable",
                                      ident->val.c_str());
      }
      auto value = std::make_unique<hir::Variable>(ident->GetPos());
      value->decl = decl;
      return std::move(value);
    } break;

    case ast::Expr::Kind::UNARY: {
      auto unaryExpr = (ast::UnaryExpr*)expr;
      auto e = MakeExpr(unaryExpr->expr.get());
      if (e->IsConstant()) {
        MakeConstUnary((hir::Constant*)e.get(), unaryExpr->unOp);
        return std::move(e);
      }
      return std::make_unique<hir::Unary>(unaryExpr->GetPos(), unaryExpr->unOp,
                                          std::move(e));
    } break;

    case ast::Expr::Kind::BINARY: {
      auto binaryExpr = (ast::BinaryExpr*)expr;
      auto lhs = MakeExpr(binaryExpr->lhs.get());
      auto rhs = MakeExpr(binaryExpr->rhs.get());
      if (lhs->IsConstant() && rhs->IsConstant()) {
        auto lCons = (hir::Constant*)lhs.get();
        auto rCons = (hir::Constant*)rhs.get();
        return MakeConstBinary(lCons, rCons, binaryExpr->op);
      }
      CheckBinary(lhs, rhs, binaryExpr->op);
      return std::make_unique<hir::Binary>(binaryExpr->GetPos(), binaryExpr->op,
                                           std::move(lhs), std::move(rhs));
    } break;
    default:
      return nullptr;
  }
}

void Checker::MakeConstUnary(hir::Constant* cons, ast::UnOp op) {
  switch (op) {
    case ast::UnOp::NOT:
      if (cons->Ty()->IsBool()) {
        auto b = (hir::BoolConstant*)cons;
        b->val = !b->val;
      } else {
        throw CompileError::CreatePos(cons->pos, "non bool type");
      }
    case ast::UnOp::NEG:
      if (cons->Ty()->IsNumeric()) {
        if (cons->Ty()->IsI32() || cons->Ty()->IsI64()) {
          auto b = (hir::IntConstant*)cons;
          b->val = -b->val;
        } else {
          auto b = (hir::FloatConstant*)cons;
          b->val = -b->val;
        }
      } else {
        throw CompileError::CreatePos(cons->pos, "non numeric type");
      }
  }
}

std::unique_ptr<hir::Constant> Checker::MakeConstBinary(hir::Constant* lhs,
                                                        hir::Constant* rhs,
                                                        ast::BinOp op) {
  switch (lhs->ConstantKind()) {
    case hir::Constant::Kind::INT: {
      auto l = (hir::IntConstant*)lhs;
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // int int
          auto r = (hir::IntConstant*)rhs;
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val / r->val);
            case ast::BinOp::MOD:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val % r->val);
          }
        } break;

        case hir::Constant::Kind::FLOAT: {  // int float
          auto r = (hir::FloatConstant*)rhs;
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val / r->val);
            case ast::BinOp::MOD:
              throw CompileError::CreatePos(
                  r->pos, "operator \% not defined on untyped float");
          }
        } break;

        case hir::Constant::Kind::CHAR: {  // int char
          auto r = (hir::CharConstant*)rhs;
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val / r->val);
            case ast::BinOp::MOD:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val % r->val);
          }
        } break;
        default:
          throw CompileError::CreatePos(lhs->pos, "cannot binary");
      }
    } break;

    case hir::Constant::Kind::FLOAT: {
      auto l = (hir::FloatConstant*)lhs;
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // float int
          auto r = (hir::IntConstant*)rhs;
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val / r->val);
            case ast::BinOp::MOD:
              throw CompileError::CreatePos(
                  l->pos, "operator \% not defined on untyped float");
          }
        } break;
        case hir::Constant::Kind::FLOAT: {  // float float
          auto r = (hir::FloatConstant*)rhs;
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val / r->val);
            case ast::BinOp::MOD:
              throw CompileError::CreatePos(
                  l->pos, "operator \% not defined on untyped float");
          }
        } break;

        case hir::Constant::Kind::CHAR: {  // float char
          auto r = (hir::CharConstant*)rhs;
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val / r->val);
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
      auto l = (hir::CharConstant*)lhs;
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // char int
          auto r = (hir::IntConstant*)rhs;
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val / r->val);
            case ast::BinOp::MOD:
              return std::make_unique<hir::IntConstant>(lhs->pos,
                                                        l->val % r->val);
          }
        } break;
        case hir::Constant::Kind::FLOAT: {  // char float
          auto r = (hir::FloatConstant*)rhs;
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::FloatConstant>(lhs->pos,
                                                          l->val / r->val);
            case ast::BinOp::MOD:
              throw CompileError::CreatePos(
                  r->pos, "operator \% not defined on untyped float");
          }
        } break;
        case hir::Constant::Kind::CHAR: {  // char char
          auto r = (hir::CharConstant*)rhs;
          switch (op) {
            case ast::BinOp::LT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val < r->val);
            case ast::BinOp::LE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val <= r->val);
            case ast::BinOp::GT:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val > r->val);
            case ast::BinOp::GE:
              return std::make_unique<hir::BoolConstant>(lhs->pos,
                                                         l->val >= r->val);

            case ast::BinOp::ADD:
              return std::make_unique<hir::CharConstant>(lhs->pos,
                                                         l->val + r->val);
            case ast::BinOp::SUB:
              return std::make_unique<hir::CharConstant>(lhs->pos,
                                                         l->val - r->val);

            case ast::BinOp::MUL:
              return std::make_unique<hir::CharConstant>(lhs->pos,
                                                         l->val * r->val);
            case ast::BinOp::DIV:
              return std::make_unique<hir::CharConstant>(lhs->pos,
                                                         l->val / r->val);
            case ast::BinOp::MOD:
              return std::make_unique<hir::CharConstant>(lhs->pos,
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

void Checker::CheckBinary(std::unique_ptr<hir::Expr>& lhs,
                          std::unique_ptr<hir::Expr>& rhs, ast::BinOp op) {
  auto lhsTy = lhs->Ty();
  auto rhsTy = rhs->Ty();

  if (!lhsTy->IsNumeric()) {
    throw CompileError::CreatePos(lhs->pos, "lhs is not numeric type");
  }
  if (!rhsTy->IsNumeric()) {
    throw CompileError::CreatePos(rhs->pos, "rhs is not numeric type");
  }

  if (*lhsTy == *rhsTy) return;

  switch (op) {
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
            lhs->pos, "operator \% not defined on untyped float");
      if (!rhsTy->IsI32() && !rhsTy->IsI64() && !rhsTy->IsChar())
        throw CompileError::CreatePos(
            rhs->pos, "operator \% not defined on untyped float");
      break;
  }

  // No possibility Constant + Constant because it should be treated in
  // MakeConstBinary. If lhs is constant, rhs should not be constant so
  // rhs is preferred.
  bool isRightPrior = lhs->IsConstant();
  if (isRightPrior) {
    TryExpTy(lhs.get(), rhsTy);
  } else {
    TryExpTy(rhs.get(), lhsTy);
  }
}

void Checker::TryConstantTy(hir::Constant* cons, std::shared_ptr<Type> ty) {
  switch (cons->ConstantKind()) {
    case hir::Constant::Kind::CHAR: {
      // char may be int or float
      auto charConst = (hir::CharConstant*)cons;
      switch (ty->TypeKind()) {
        case Type::CHAR:
          return;
        case Type::I32:
        case Type::I64: {
          auto rune = charConst->val;
          auto pos = charConst->pos;
          delete charConst;
          cons = new hir::IntConstant(pos, rune);
          return;
        } break;

        case Type::F32:
        case Type::F64: {
          auto rune = charConst->val;
          auto pos = charConst->pos;
          delete charConst;
          cons = new hir::FloatConstant(pos, rune);
          return;
        } break;

        default:
          throw CompileError::CreatePos(cons->pos, "cannot cast char literal");
      }
    } break;

    case hir::Constant::Kind::INT: {
      auto intConst = (hir::IntConstant*)cons;
      switch (ty->TypeKind()) {
        case Type::CHAR: {
          if (intConst->is32) {
            auto val = intConst->val;
            auto pos = intConst->pos;
            delete intConst;
            cons = new hir::CharConstant(pos, val);
            return;
          }

          throw CompileError::CreatePos(cons->pos, "overflow char");
        } break;

        case Type::I32: {
          if (intConst->is32) return;

          if (intConst->val > INT32_MAX)
            throw CompileError::CreatePos(cons->pos, "overflow int32");

          intConst->is32 = true;
        } break;

        case Type::I64:
          break;

        case Type::F32: {
          if (intConst->is32) {
            auto val = intConst->val;
            auto pos = intConst->pos;
            delete intConst;
            cons = new hir::FloatConstant(pos, val);
            return;
          }

          throw CompileError::CreatePos(cons->pos, "overflow f32");
        } break;

        case Type::F64: {
          auto rune = intConst->val;
          auto pos = intConst->pos;
          delete intConst;
          cons = new hir::FloatConstant(pos, rune);
          return;
        } break;

        default:
          throw CompileError::CreatePos(cons->pos, "can't cast");
      }
    } break;

    case hir::Constant::Kind::BOOL: {
      if (!ty->IsBool())
        throw CompileError::CreatePos(cons->pos, "can't cast bool");
    } break;

    case hir::Constant::Kind::FLOAT: {
      if (!ty->IsF32())
        throw CompileError::CreatePos(cons->pos, "can't cast f32");
    } break;

    case hir::Constant::Kind::STRING: {
      if (!ty->IsString())
        throw CompileError::CreatePos(cons->pos, "can't cast string");
    } break;
  }
}

// check exp's type and try to set type `ty`
void Checker::TryExpTy(hir::Expr* expr, std::shared_ptr<Type> ty) {
  switch (expr->ExprKind()) {
    case hir::Expr::Kind::VALUE: {
      auto value = (hir::Value*)expr;
      switch (value->ValueKind()) {
        case hir::Value::Kind::VARIABLE: {
          // Variables can't be casted implicitly
          auto var = (hir::Variable*)value;
          if (*var->decl->type != *ty) {
            throw CompileError::CreatePos(expr->pos, "unmatched variable type");
          }
        } break;
        case hir::Value::Kind::CONSTANT: {
          // Constants can be casted implicitly
          auto cons = (hir::Constant*)value;
          TryConstantTy(cons, ty);
        } break;
      }
    } break;

    case hir::Expr::Kind::BINARY: {
      auto binary = (hir::Binary*)expr;
      if (*binary->Ty() == *ty) return;
      TryExpTy(binary->lhs.get(), ty);
      TryExpTy(binary->rhs.get(), ty);
      if (*binary->Ty() != *ty)
        throw CompileError::CreatePos(binary->pos, "unmatched binary type");
    } break;
    default: {
      if (*expr->Ty() != *ty) {
        throw CompileError::CreatePos(expr->pos, "unmatched exp type");
      }
    } break;
  }
}

std::unique_ptr<hir::Constant> Checker::MakeLit(ast::Lit* lit) {
  switch (lit->LitKind()) {
    case ast::Lit::Kind::INT:
      return ParseInt(lit);

    case ast::Lit::Kind::FLOAT: {
      auto val = ParseFloat(lit);
      return std::make_unique<hir::FloatConstant>(lit->GetPos(), val);
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

std::unique_ptr<hir::IntConstant> Checker::ParseInt(ast::Lit* lit) {
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

double Checker::ParseFloat(ast::Lit* lit) {
  // TODO:parse float
  throw CompileError::CreatePos(lit->GetPos(), "unimplemented parse float");
  return 0;
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
