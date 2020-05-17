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
  auto hir_file = std::make_unique<hir::File>();
  while (!file->externs.empty()) {
    auto ext = file->externs.move_front();
    auto decl = ast_decl_.at(ext.get());
    hir_file->externs.push_back(
        std::make_unique<hir::Extern>(ext->Begin(), ext->End(), decl));
  }
  while (!file->fn_decls.empty()) {
    auto fn = file->fn_decls.move_front();
    auto decl = ast_decl_.at(fn.get());
    current_func_ = decl->AsFuncType();

    std::deque<std::shared_ptr<Decl>> args;
    for (auto& arg : fn->proto->args->list) {
      args.push_back(ast_decl_.at(arg.get()));
    }
    auto block = LowerBlock(std::move(fn->block));

    if (*block->Ty() != *current_func_->ret) {
      Loc loc;
      if (block->stmts.empty()) {
        loc = block->End();
      } else {
        loc = block->stmts.back()->Begin();
      }
      throw LocError(loc, "unmatched return type");
    }

    hir_file->fn_decls.push_back(std::make_unique<hir::FnDecl>(
        fn->Begin(), decl, args, std::move(block)));
  }
  return std::move(hir_file);
}

std::unique_ptr<hir::Stmt> Lower::LowerStmt(std::unique_ptr<ast::Stmt> stmt) {
  std::cout << ToString(stmt->StmtKind()) << " " << stmt.get() << std::endl;
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
  auto begin = stmt->Begin();
  if (stmt->expr) {
    auto expr = LowerExpr(std::move(stmt->expr));
    expr = TryExprTy(std::move(expr), current_func_->ret);
    return std::make_unique<hir::RetStmt>(begin, std::move(expr));
  } else {
    return std::make_unique<hir::RetStmt>(begin);
  }
}

bool IsAllTerminated(hir::If* root) {
  assert(!root->MissingElse());
  bool else_terminated;
  if (root->IsElseBlock()) {
    auto els = (hir::Block*)root->els.get();
    else_terminated = els->HasRet();
  } else {
    auto els = (hir::If*)root->els.get();
    else_terminated = IsAllTerminated(els);
  }
  return root->block->HasRet() && else_terminated;
}

void Lower::CheckNotVoidType(const hir::Expr* expr) {
  if (!expr->Ty()->IsVoid()) return;

  switch (expr->ExprKind()) {
    case hir::Expr::Kind::IF: {
      auto& if_stmt = (std::unique_ptr<hir::If>&)expr;
      if (if_stmt->MissingElse())
        throw LocError(if_stmt->End(), "missing else clause");

      if (IsAllTerminated(if_stmt.get()))
        throw LocError(if_stmt->Begin(), "all ret if");

    } break;
    case hir::Expr::Kind::BLOCK: {
      auto& block = (std::unique_ptr<hir::Block>&)expr;
      if (block->stmts.empty()) throw LocError(block->End(), "no statements");
      if (block->HasRet())
        throw LocError(block->End(), "block contains ret statement");
    } break;
    default:
      break;
  }
  throw LocError::Create(expr->Begin(), "cannot use void type");
}

std::unique_ptr<hir::VarDeclStmt> Lower::LowerVarDecl(
    std::unique_ptr<ast::VarDeclStmt> stmt) {
  auto begin = stmt->Begin();
  auto decl = ast_decl_.at(stmt.get());
  assert(decl->type->IsUnresolved());
  auto expr = LowerExpr(std::move(stmt->expr));
  CheckNotVoidType(expr.get());
  decl->type = expr->Ty();
  decl->Debug();
  return std::make_unique<hir::VarDeclStmt>(begin, decl, std::move(expr));
}

std::unique_ptr<hir::AssignStmt> Lower::LowerAssign(
    std::unique_ptr<ast::AssignStmt> stmt) {
  auto begin = stmt->Begin();
  auto decl = ast_decl_.at(stmt.get());
  auto expr = LowerExpr(std::move(stmt->expr));
  expr = TryExprTy(std::move(expr), decl->type);
  return std::make_unique<hir::AssignStmt>(begin, decl, std::move(expr));
}

std::unique_ptr<hir::Expr> Lower::LowerExpr(std::unique_ptr<ast::Expr> expr) {
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::LIT:
      return LowerLit(unique_cast<ast::Lit>(std::move(expr)));

    case ast::Expr::Kind::CALL: {
      auto call = unique_cast<ast::CallExpr>(std::move(expr));
      auto begin = call->Begin();
      auto end = call->End();
      auto decl = ast_decl_.at(call.get());
      auto func_decl = decl->AsFuncType();
      unique_deque<hir::Expr> args;
      while (!call->args.empty()) {
        auto arg = call->args.move_front();
        auto expr = LowerExpr(std::move(arg));
        auto idx = args.size();
        auto ty = func_decl->args[idx];
        expr = TryExprTy(std::move(expr), ty);
        args.push_back(std::move(expr));
      }
      return std::make_unique<hir::Call>(begin, end, decl, std::move(args));
    } break;

    case ast::Expr::Kind::IDENT: {
      auto decl = ast_decl_.at(expr.get());
      auto begin = expr->Begin();
      auto end = expr->End();
      return std::make_unique<hir::Variable>(begin, end, decl);
    } break;

    case ast::Expr::Kind::UNARY: {
      auto unary = unique_cast<ast::UnaryExpr>(std::move(expr));
      auto begin = unary->Begin();
      auto end = unary->End();
      auto op = un_op_ast_to_hir(unary->op->op);
      auto expr = LowerExpr(std::move(unary->expr));
      if (expr->IsConstant()) {
        return ConstUnary(unique_cast<hir::Constant>(std::move(expr)), op);
      }
      return std::make_unique<hir::Unary>(begin, end, op, std::move(expr));
    } break;

    case ast::Expr::Kind::BINARY: {
      auto binary = unique_cast<ast::BinaryExpr>(std::move(expr));
      auto begin = binary->Begin();
      auto end = binary->End();
      auto op = bin_op_ast_to_hir(binary->op->op);
      auto lhs = LowerExpr(std::move(binary->lhs));
      auto rhs = LowerExpr(std::move(binary->rhs));
      return CheckBinary(std::make_unique<hir::Binary>(
          begin, end, op, std::move(lhs), std::move(rhs)));
    } break;
    case ast::Expr::Kind::IF:
      return LowerIf(unique_cast<ast::If>(std::move(expr)));
    case ast::Expr::Kind::BLOCK:
      return LowerBlock(unique_cast<ast::Block>(std::move(expr)));
  }
}

std::unique_ptr<hir::If> Lower::LowerIf(std::unique_ptr<ast::If> expr) {
  auto begin = expr->Begin();
  auto end = expr->End();
  auto cond = LowerExpr(std::move(expr->cond));
  if (!cond->Ty()->IsBool()) {
    throw LocError::Create(cond->Begin(), "non bool if cond");
  }
  auto block = LowerBlock(std::move(expr->block));

  if (expr->HasElse()) {
    if (expr->IsElseIf()) {
      auto els = LowerIf(unique_cast<ast::If>(std::move(expr->els)));
      return std::make_unique<hir::If>(begin, end, std::move(cond),
                                       std::move(block), std::move(els));
    } else if (expr->IsElseBlock()) {
      auto els = LowerBlock(unique_cast<ast::Block>(std::move(expr->els)));
      return std::make_unique<hir::If>(begin, end, std::move(cond),
                                       std::move(block), std::move(els));
    }
  }
  return std::make_unique<hir::If>(begin, end, std::move(cond),
                                   std::move(block));
}

std::unique_ptr<hir::Block> Lower::LowerBlock(
    std::unique_ptr<ast::Block> block) {
  Loc begin = block->Begin();
  Loc end = block->End();
  unique_deque<hir::Stmt> stmts;
  while (!block->stmts.empty()) {
    stmts.push_back(LowerStmt(block->stmts.move_front()));
  }
  return std::make_unique<hir::Block>(begin, end, std::move(stmts));
}

std::unique_ptr<hir::Constant> Lower::LowerLit(std::unique_ptr<ast::Lit> lit) {
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

std::unique_ptr<hir::IntConstant> Lower::ParseInt(
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

double Lower::ParseFloat(std::unique_ptr<ast::Lit> lit) {
  try {
    // TODO: parse float
    double n = stod(lit->val);
    return n;
  } catch (std::out_of_range e) {
    throw LocError::Create(lit->Begin(), "out of range");
  } catch (std::invalid_argument e) {
    throw LocError::Create(lit->Begin(), "invalid or unimplemented");
  }
}

std::unique_ptr<hir::Constant> Lower::ConstBinary(
    std::unique_ptr<hir::Constant> lhs, std::unique_ptr<hir::Constant> rhs,
    hir::Binary::Op op) {
  auto lhs_begin = lhs->Begin();
  auto rhs_end = rhs->End();
  switch (lhs->ConstantKind()) {
    case hir::Constant::Kind::INT: {
      auto l = unique_cast<hir::IntConstant>(std::move(lhs));
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // int int
          auto r = unique_cast<hir::IntConstant>(std::move(rhs));
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
          auto r = unique_cast<hir::FloatConstant>(std::move(rhs));
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
          auto r = unique_cast<hir::CharConstant>(std::move(rhs));
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
      auto l = unique_cast<hir::FloatConstant>(std::move(lhs));
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // float int
          auto r = unique_cast<hir::IntConstant>(std::move(rhs));
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
          auto r = unique_cast<hir::FloatConstant>(std::move(rhs));
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
          auto r = unique_cast<hir::CharConstant>(std::move(rhs));
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
      auto l = unique_cast<hir::CharConstant>(std::move(lhs));
      switch (rhs->ConstantKind()) {
        case hir::Constant::Kind::INT: {  // char int
          auto r = unique_cast<hir::IntConstant>(std::move(rhs));
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
          auto r = unique_cast<hir::FloatConstant>(std::move(rhs));
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
          auto r = unique_cast<hir::CharConstant>(std::move(rhs));
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
}

std::unique_ptr<hir::Expr> Lower::TryConstantTy(
    std::unique_ptr<hir::Constant> cons, std::shared_ptr<Type> ty) {
  auto begin = cons->Begin();
  auto end = cons->End();
  switch (cons->ConstantKind()) {
    case hir::Constant::Kind::CHAR: {
      // char may be int or float
      auto char_const = unique_cast<hir::CharConstant>(std::move(cons));
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
      auto int_const = unique_cast<hir::IntConstant>(std::move(cons));
      switch (ty->TypeKind()) {
        case Type::CHAR: {
          if (int_const->is_32) {
            auto val = int_const->val;
            return std::make_unique<hir::CharConstant>(begin, end, val);
          }

          throw LocError::Create(begin, "overflow char");
        } break;

        case Type::I32: {
          if (int_const->is_32) return std::move(int_const);

          if (int_const->val > INT32_MAX)
            throw LocError::Create(begin, "overflow int32");

          int_const->is_32 = true;
          return std::move(int_const);
        } break;

        case Type::I64:
          return std::move(int_const);

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
      auto float_const = unique_cast<hir::FloatConstant>(std::move(cons));
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

std::unique_ptr<hir::Expr> Lower::CheckBinary(
    std::unique_ptr<hir::Binary> binary) {
  if (binary->lhs->IsConstant() && binary->rhs->IsConstant()) {
    return ConstBinary(unique_cast<hir::Constant>(std::move(binary->lhs)),
                       unique_cast<hir::Constant>(std::move(binary->rhs)),
                       binary->op);
  }
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

  bool is_right_prior = binary->lhs->IsConstant();
  if (is_right_prior) {
    binary->lhs = TryExprTy(std::move(binary->lhs), rhs_ty);
  } else {
    binary->rhs = TryExprTy(std::move(binary->rhs), lhs_ty);
  }
  return std::move(binary);
}

std::unique_ptr<hir::Constant> Lower::ConstUnary(
    std::unique_ptr<hir::Constant> cons, hir::Unary::Op op) {
  switch (op) {
    case hir::Unary::Op::NOT:
      if (cons->Ty()->IsBool()) {
        auto b = unique_cast<hir::BoolConstant>(std::move(cons));
        b->val = !b->val;
        return std::move(b);
      } else {
        throw LocError::Create(cons->Begin(), "non bool type");
      }
    case hir::Unary::Op::NEG:
      if (cons->Ty()->IsNumeric()) {
        if (cons->Ty()->IsI32() || cons->Ty()->IsI64()) {
          auto b = unique_cast<hir::IntConstant>(std::move(cons));
          b->val = -b->val;
          return b;
        } else {
          auto b = unique_cast<hir::FloatConstant>(std::move(cons));
          b->val = -b->val;
          return b;
        }
      } else {
        throw LocError::Create(cons->Begin(), "non numeric type");
      }
  }
}

// check exp's type and try to set type `ty`
std::unique_ptr<hir::Expr> Lower::TryExprTy(std::unique_ptr<hir::Expr> expr,
                                            std::shared_ptr<Type> ty) {
  switch (expr->ExprKind()) {
    case hir::Expr::Kind::VALUE: {
      CheckNotVoidType(expr.get());
      auto value = unique_cast<hir::Value>(std::move(expr));
      switch (value->ValueKind()) {
        case hir::Value::Kind::VARIABLE: {
          // Variables can't be casted implicitly
          auto var = unique_cast<hir::Variable>(std::move(value));
          var->decl->Debug();
          if (*var->decl->type != *ty) {
            throw LocError::Create(expr->Begin(), "unmatched variable type");
          }
          return std::move(var);
        } break;
        case hir::Value::Kind::CONSTANT: {
          // Constants can be casted implicitly
          auto cons = unique_cast<hir::Constant>(std::move(value));
          return TryConstantTy(std::move(cons), ty);
        } break;
      }
    } break;

    case hir::Expr::Kind::BINARY: {
      CheckNotVoidType(expr.get());
      auto binary = unique_cast<hir::Binary>(std::move(expr));
      if (*binary->Ty() == *ty) return std::move(binary);
      binary->lhs = TryExprTy(std::move(binary->lhs), ty);
      binary->rhs = TryExprTy(std::move(binary->rhs), ty);
      return std::move(binary);
    } break;

    case hir::Expr::Kind::CALL:
    case hir::Expr::Kind::UNARY:
      CheckNotVoidType(expr.get());
      if (*expr->Ty() != *ty) {
        throw LocError::Create(expr->Begin(), "unmatched exp type");
      }
      return std::move(expr);
    case hir::Expr::Kind::IF: {
      CheckNotVoidType(expr.get());
      auto if_stmt = unique_cast<hir::If>(std::move(expr));

      if_stmt->block =
          unique_cast<hir::Block>(TryExprTy(std::move(if_stmt->block), ty));

      if (if_stmt->IsElseIf()) {
        if_stmt->els =
            unique_cast<hir::If>(TryExprTy(std::move(if_stmt->els), ty));
      } else if (if_stmt->IsElseBlock()) {
        if_stmt->els =
            unique_cast<hir::Block>(TryExprTy(std::move(if_stmt->els), ty));
      }
      return std::move(if_stmt);
    } break;
    case hir::Expr::Kind::BLOCK: {
      auto block = unique_cast<hir::Block>(std::move(expr));
      if (block->HasRet()) {
        // ret block doesn't need to check type
        return std::move(block);
      }
      CheckNotVoidType(block.get());
      auto last = block->stmts.move_back();
      if (last->StmtKind() == hir::Stmt::Kind::EXPR) {
        last = TryExprTy(unique_cast<hir::Expr>(std::move(last)), ty);
        block->stmts.push_back(std::move(last));
        return std::move(block);
      } else {
        throw LocError(last->Begin(), "block is empty");
      }
    } break;
  };
}

}  // namespace felis
