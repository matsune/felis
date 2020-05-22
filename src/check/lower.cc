#include "check/lower.h"

#include <assert.h>

#include "error/error.h"
#include "unique.h"

namespace felis {

namespace {

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
    case ast::BinaryOp::Op::EQEQ:
      return hir::Binary::Op::EQEQ;
    case ast::BinaryOp::Op::NEQ:
      return hir::Binary::Op::NEQ;
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

}  // namespace

std::unique_ptr<hir::File> Lower::Lowering(std::unique_ptr<ast::File> file) {
  // Type Checks
  decl_checker_.Check(file);
  ty_infer_.Infer(file);

  node_map_.FinalizeTypes();
  node_map_.Debug();

  // Lowering AST to HIR
  auto hir_file = std::make_unique<hir::File>();
  while (!file->externs.empty()) {
    auto ext = file->externs.move_front();
    auto decl = node_map_.GetDecl(ext->proto->name);
    hir_file->externs.push_back(std::make_unique<hir::Extern>(decl));
  }
  while (!file->fn_decls.empty()) {
    auto fn = file->fn_decls.move_front();
    auto decl = node_map_.GetDecl(fn->proto->name);

    std::deque<std::shared_ptr<Decl>> args;
    for (auto& arg : fn->proto->args->list) {
      auto arg_decl = node_map_.GetDecl(arg->name);
      args.push_back(arg_decl);
    }
    auto block = LowerBlock(std::move(fn->block));

    hir_file->fn_decls.push_back(
        std::make_unique<hir::FnDecl>(decl, args, std::move(block)));
  }
  return std::move(hir_file);
}

std::unique_ptr<hir::Stmt> Lower::LowerStmt(std::unique_ptr<ast::Stmt> stmt) {
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      return LowerExpr(unique_cast<ast::Expr>(std::move(stmt)));
    case ast::Stmt::Kind::RET:
      return LowerRet(unique_cast<ast::RetStmt>(std::move(stmt)));
    case ast::Stmt::Kind::VAR_DECL:
      return LowerVarDecl(unique_cast<ast::VarDeclStmt>(std::move(stmt)));
    case ast::Stmt::Kind::ASSIGN:
      return LowerAssign(unique_cast<ast::AssignStmt>(std::move(stmt)));
  }
}

std::unique_ptr<hir::RetStmt> Lower::LowerRet(
    std::unique_ptr<ast::RetStmt> stmt) {
  if (stmt->expr) {
    return std::make_unique<hir::RetStmt>(LowerExpr(std::move(stmt->expr)));
  } else {
    return std::make_unique<hir::RetStmt>();
  }
}

std::unique_ptr<hir::VarDeclStmt> Lower::LowerVarDecl(
    std::unique_ptr<ast::VarDeclStmt> stmt) {
  auto decl = node_map_.GetDecl(stmt->name);
  auto expr = LowerExpr(std::move(stmt->expr));
  return std::make_unique<hir::VarDeclStmt>(decl, std::move(expr));
}

std::unique_ptr<hir::AssignStmt> Lower::LowerAssign(
    std::unique_ptr<ast::AssignStmt> stmt) {
  auto decl = node_map_.GetDecl(stmt->name);
  auto expr = LowerExpr(std::move(stmt->expr));
  return std::make_unique<hir::AssignStmt>(decl, std::move(expr));
}

std::unique_ptr<hir::Expr> Lower::LowerExpr(std::unique_ptr<ast::Expr> expr) {
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT:
      return LowerLit(unique_cast<ast::Lit>(std::move(expr)));

    case ast::Expr::Kind::CALL: {
      auto call = unique_cast<ast::CallExpr>(std::move(expr));
      auto decl = node_map_.GetDecl(call->ident);
      auto func_decl = decl->AsFuncType();
      unique_deque<hir::Expr> args;
      while (!call->args.empty()) {
        auto arg = call->args.move_front();
        args.push_back(LowerExpr(std::move(arg)));
      }
      return std::make_unique<hir::Call>(decl, std::move(args));
    } break;

    case ast::Expr::Kind::IDENT: {
      auto ident = unique_cast<ast::Ident>(std::move(expr));
      auto decl = node_map_.GetDecl(ident);
      return std::make_unique<hir::Variable>(decl);
    } break;

    case ast::Expr::Kind::UNARY: {
      auto unary = unique_cast<ast::UnaryExpr>(std::move(expr));
      auto op = un_op_ast_to_hir(unary->op->op);
      auto expr = LowerExpr(std::move(unary->expr));
      return std::make_unique<hir::Unary>(op, std::move(expr));
    } break;

    case ast::Expr::Kind::BINARY: {
      auto ty = node_map_.GetType(expr);
      auto binary = unique_cast<ast::BinaryExpr>(std::move(expr));
      auto op = bin_op_ast_to_hir(binary->op->op);
      auto lhs = LowerExpr(std::move(binary->lhs));
      auto rhs = LowerExpr(std::move(binary->rhs));
      return std::make_unique<hir::Binary>(ty, op, std::move(lhs),
                                           std::move(rhs));
    } break;
    case ast::Expr::Kind::IF:
      return LowerIf(unique_cast<ast::If>(std::move(expr)));
    case ast::Expr::Kind::BLOCK:
      return LowerBlock(unique_cast<ast::Block>(std::move(expr)));
  }
}

std::unique_ptr<hir::If> Lower::LowerIf(std::unique_ptr<ast::If> expr) {
  auto ty = node_map_.GetType(expr);
  auto cond = LowerExpr(std::move(expr->cond));
  auto block = LowerBlock(std::move(expr->block));
  std::unique_ptr<hir::Expr> els = nullptr;
  if (expr->HasElse()) {
    if (expr->IsElseIf()) {
      els = LowerIf(unique_cast<ast::If>(std::move(expr->els)));
    } else {
      els = LowerBlock(unique_cast<ast::Block>(std::move(expr->els)));
    }
  }
  return std::make_unique<hir::If>(ty, std::move(cond), std::move(block),
                                   std::move(els));
}

std::unique_ptr<hir::Block> Lower::LowerBlock(
    std::unique_ptr<ast::Block> block) {
  auto ty = node_map_.GetType(block);
  unique_deque<hir::Stmt> stmts;
  while (!block->stmts.empty()) {
    stmts.push_back(LowerStmt(block->stmts.move_front()));
  }
  return std::make_unique<hir::Block>(ty, std::move(stmts));
}

std::unique_ptr<hir::Value> Lower::LowerLit(std::unique_ptr<ast::Lit> lit) {
  switch (lit->LitKind()) {
    case ast::Lit::Kind::CHAR: {
      auto ty = node_map_.GetType(lit);

      std::stringstream ss(lit->val);
      rune r;
      ss >> r;
      switch (ty->TypedKind()) {
        case Typed::Kind::I32:
        case Typed::Kind::I64:
          return std::make_unique<hir::IntConstant>(ty, r);
        case Typed::Kind::F32:
        case Typed::Kind::F64:
          return std::make_unique<hir::FloatConstant>(ty, r);
        default:
          UNREACHABLE
      }

    } break;
    case ast::Lit::Kind::INT:
      return ParseInt(std::move(lit));
    case ast::Lit::Kind::FLOAT: {
      return ParseFloat(std::move(lit));
    } break;
    case ast::Lit::Kind::BOOL: {
      auto ty = node_map_.GetType(lit);
      return std::make_unique<hir::BoolConstant>(ty, lit->val == "true");
    } break;
    case ast::Lit::Kind::STRING: {
      auto ty = node_map_.GetType(lit);
      return std::make_unique<hir::StringConstant>(ty, lit->val);
    } break;
  }
}

std::unique_ptr<hir::Value> Lower::ParseInt(std::unique_ptr<ast::Lit> lit) {
  try {
    int64_t n = stoll(lit->val);
    auto ty = node_map_.GetType(lit);
    switch (ty->TypedKind()) {
      case Typed::Kind::I32:
        if (n > INT32_MAX) {
          throw LocError::Create(lit->Begin(), "overflow int32");
        }
      case Typed::Kind::I64:
        return std::make_unique<hir::IntConstant>(ty, n);
      case Typed::Kind::F32:
      case Typed::Kind::F64:
        return std::make_unique<hir::FloatConstant>(ty, n);
      default:
        UNREACHABLE
    }

  } catch (std::out_of_range e) {
    throw LocError::Create(lit->Begin(), "out of range");
  } catch (std::invalid_argument e) {
    throw LocError::Create(lit->Begin(), "invalid or unimplemented");
  }
}

std::unique_ptr<hir::FloatConstant> Lower::ParseFloat(
    std::unique_ptr<ast::Lit> lit) {
  try {
    // TODO: parse float
    double n = stod(lit->val);
    auto ty = node_map_.GetType(lit);
    switch (ty->TypedKind()) {
      case Typed::Kind::F32:
      case Typed::Kind::F64:
        break;
      default:
        UNREACHABLE
    }
    return std::make_unique<hir::FloatConstant>(ty, n);
  } catch (std::out_of_range e) {
    throw LocError::Create(lit->Begin(), "out of range");
  } catch (std::invalid_argument e) {
    throw LocError::Create(lit->Begin(), "invalid or unimplemented");
  }
}

}  // namespace felis
