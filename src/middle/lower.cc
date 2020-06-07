
#include "lower.h"

#include <sstream>

#include "check/parse.h"
#include "check/type_checker.h"
#include "error/error.h"
#include "macro.h"

namespace felis {

namespace {

bool IsBinOp(ast::BinaryOp::Op op) {
  switch (op) {
    case ast::BinaryOp::Op::ADD:
    case ast::BinaryOp::Op::SUB:
    case ast::BinaryOp::Op::MUL:
    case ast::BinaryOp::Op::DIV:
    case ast::BinaryOp::Op::MOD:
      return true;
    default:
      return false;
  }
}

bool IsCmpOp(ast::BinaryOp::Op op) {
  switch (op) {
    case ast::BinaryOp::Op::EQEQ:
    case ast::BinaryOp::Op::NEQ:
    case ast::BinaryOp::Op::LT:
    case ast::BinaryOp::Op::LE:
    case ast::BinaryOp::Op::GT:
    case ast::BinaryOp::Op::GE:
      return true;
    default:
      return false;
  }
}

mir::BinaryInst::Op BinOp(ast::BinaryOp::Op op) {
  switch (op) {
    case ast::BinaryOp::Op::ADD:
      return mir::BinaryInst::Op::ADD;
    case ast::BinaryOp::Op::SUB:
      return mir::BinaryInst::Op::SUB;
    case ast::BinaryOp::Op::MUL:
      return mir::BinaryInst::Op::MUL;
    case ast::BinaryOp::Op::DIV:
      return mir::BinaryInst::Op::DIV;
    case ast::BinaryOp::Op::MOD:
      return mir::BinaryInst::Op::MOD;
    default:
      UNREACHABLE
  }
}

mir::CmpInst::Op CmpOp(ast::BinaryOp::Op op) {
  switch (op) {
    case ast::BinaryOp::Op::EQEQ:
      return mir::CmpInst::Op::EQEQ;
    case ast::BinaryOp::Op::NEQ:
      return mir::CmpInst::Op::NEQ;
    case ast::BinaryOp::Op::LT:
      return mir::CmpInst::Op::LT;
    case ast::BinaryOp::Op::LE:
      return mir::CmpInst::Op::LE;
    case ast::BinaryOp::Op::GT:
      return mir::CmpInst::Op::GT;
    case ast::BinaryOp::Op::GE:
      return mir::CmpInst::Op::GE;
    default:
      UNREACHABLE
  }
}

mir::UnaryInst::Op UnaryOp(ast::UnaryOp::Kind op) {
  switch (op) {
    case ast::UnaryOp::NEG:
      return mir::UnaryInst::Op::NEG;
    case ast::UnaryOp::NOT:
      return mir::UnaryInst::Op::NOT;
  }
}

}  // namespace

std::unique_ptr<mir::File> Lowering(std::unique_ptr<ast::File> file,
                                    bool is_32bit) {
  TypeCheckCtx ctx(is_32bit);
  TypeChecker(ctx).Check(file);
  ctx.FinalizeType();

  auto mir_file = std::make_unique<mir::File>();
  Lower(ctx, mir_file).Lowering(std::move(file));
  return std::move(mir_file);
}

void Lower::Lowering(std::unique_ptr<ast::File> file) {
  std::cout << "CreateFn ext" << std::endl;
  while (!file->externs.empty()) {
    auto ext = file->externs.move_front();
    auto decl = ctx_.GetDecl(ext->proto->name);
    builder_.CreateFunc(decl);
  }

  std::cout << "CreateFn fn" << std::endl;
  for (auto &fn_decl : file->fn_decls) {
    auto decl = ctx_.GetDecl(fn_decl->proto->name);
    auto func =
        std::dynamic_pointer_cast<mir::Function>(builder_.CreateFunc(decl));
    builder_.SetInsertBB(func->entry_bb);

    for (auto &arg : fn_decl->proto->args->list) {
      auto decl = ctx_.GetDecl(arg->name);
      auto arg_var = builder_.CreateVar(ctx_.ResolvedType(decl->type));
      func->args.push_back(arg_var);

      auto arg_alloc = builder_.CreateAlloc(decl->type, decl->name);
      builder_.SetDeclVar(decl, arg_alloc);
      builder_.CreateAssign(arg_alloc, arg_var);
    }
  }

  std::cout << "Lower Fn" << std::endl;
  while (!file->fn_decls.empty()) {
    auto fn_decl = file->fn_decls.move_front();
    auto decl = ctx_.GetDecl(fn_decl->proto->name);
    auto function = builder_.GetDeclFunc<mir::Function>(decl);
    builder_.SetInsertBB(function->entry_bb);

    auto block_res = ctx_.GetResult(fn_decl->block);
    auto val = LowerBlock(std::move(fn_decl->block));
    if (!block_res.IsRet()) {
      if (function->type->ret->IsVoid()) {
        builder_.CreateRet(nullptr);
      } else {
        builder_.CreateRet(val);
      }
    }
  }
  std::cout << "End Lowering" << std::endl;
}

std::shared_ptr<mir::Value> Lower::LowerStmt(std::unique_ptr<ast::Stmt> stmt) {
  std::cout << "LowerStmt " << ToString(stmt->StmtKind()) << std::endl;
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      return LowerExpr(unique_cast<ast::Expr>(std::move(stmt)));
    case ast::Stmt::Kind::RET:
      LowerRet(unique_cast<ast::RetStmt>(std::move(stmt)));
      break;
    case ast::Stmt::Kind::VAR_DECL:
      LowerVarDecl(unique_cast<ast::VarDeclStmt>(std::move(stmt)));
      break;
    case ast::Stmt::Kind::ASSIGN:
      LowerAssign(unique_cast<ast::AssignStmt>(std::move(stmt)));
      break;
  }
  return nullptr;
}

void Lower::LowerRet(std::unique_ptr<ast::RetStmt> stmt) {
  if (stmt->expr) {
    auto val = LowerExpr(std::move(stmt->expr));
    builder_.CreateRet(val);
  } else {
    builder_.CreateRet();
  }
}

void Lower::LowerVarDecl(std::unique_ptr<ast::VarDeclStmt> stmt) {
  auto decl = ctx_.GetDecl(stmt->name);

  auto val = LowerExpr(std::move(stmt->expr));

  if (auto var = std::dynamic_pointer_cast<mir::Var>(val)) {
    if (var->alloc) {
      builder_.SetDeclVar(decl, var);
      return;
    }
  }

  auto var = builder_.CreateAlloc(decl->type);
  builder_.SetDeclVar(decl, var);
  builder_.CreateAssign(var, val);
}

void Lower::LowerAssign(std::unique_ptr<ast::AssignStmt> stmt) {
  auto decl = ctx_.GetDecl(stmt->name);
  auto var = builder_.GetDeclVar(decl);
  auto val = LowerExpr(std::move(stmt->expr));
  builder_.CreateAssign(var, val);
}

std::shared_ptr<mir::Value> Lower::LowerExpr(std::unique_ptr<ast::Expr> expr) {
  std::cout << "LowerExpr " << ToString(expr->ExprKind()) << std::endl;
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::IDENT:
      return builder_.GetDeclVar(
          ctx_.GetDecl(unique_cast<ast::Ident>(std::move(expr))));
    case ast::Expr::Kind::LIT:
      return LowerLit(unique_cast<ast::Lit>(std::move(expr)));
    case ast::Expr::Kind::BINARY:
      return LowerBinary(unique_cast<ast::BinaryExpr>(std::move(expr)));
    case ast::Expr::Kind::CALL:
      return LowerCall(unique_cast<ast::CallExpr>(std::move(expr)));
    case ast::Expr::Kind::UNARY:
      return LowerUnary(unique_cast<ast::UnaryExpr>(std::move(expr)));
    case ast::Expr::Kind::ARRAY:
      return LowerArray(unique_cast<ast::ArrayExpr>(std::move(expr)));
    case ast::Expr::Kind::BLOCK:
      return LowerBlock(unique_cast<ast::Block>(std::move(expr)));
    case ast::Expr::Kind::IF:
      return LowerIf(unique_cast<ast::If>(std::move(expr)));
  }
}

std::shared_ptr<mir::Constant> Lower::LowerLit(std::unique_ptr<ast::Lit> lit) {
  std::shared_ptr<mir::Constant> val;
  switch (lit->LitKind()) {
    case ast::Lit::Kind::CHAR: {
      auto ty = ctx_.GetResult(lit).val;

      std::stringstream ss(lit->val);
      rune r;
      ss >> r;

      if (ty->IsI32() || ty->IsI64()) {
        val = std::make_unique<mir::ConstantInt>(ty, r);
      } else if (ty->IsF32() || ty->IsF64()) {
        val = std::make_unique<mir::ConstantFloat>(ty, r);
      } else {
        UNREACHABLE
      }

    } break;
    case ast::Lit::Kind::INT: {
      val = ParseIntLit(std::move(lit));
    } break;
    case ast::Lit::Kind::FLOAT: {
      val = ParseFloatLit(std::move(lit));
    } break;
    case ast::Lit::Kind::BOOL: {
      val = std::make_unique<mir::ConstantBool>(lit->val == "true");
    } break;
    case ast::Lit::Kind::STRING: {
      val = std::make_unique<mir::ConstantString>(lit->val);
    } break;
  }
  return val;
}

std::unique_ptr<mir::Constant> Lower::ParseIntLit(
    std::unique_ptr<ast::Lit> lit) {
  int64_t n;
  std::string err;
  if (!ParseInt(lit->val, n, err)) {
    throw LocError::Create(lit->Begin(), err);
  }

  auto ty = ctx_.GetResult(lit).val;
  if (ty->IsI8()) {
    if (n < INT8_MIN || n > INT8_MAX) {
      throw LocError::Create(lit->Begin(), "overflow int8");
    }
    return std::make_unique<mir::ConstantInt>(ty, n);
  } else if (ty->IsI16()) {
    if (n < INT16_MIN || n > INT16_MAX) {
      throw LocError::Create(lit->Begin(), "overflow int16");
    }
    return std::make_unique<mir::ConstantInt>(ty, n);
  } else if (ty->IsI32()) {
    if (n < INT32_MIN || n > INT32_MAX) {
      throw LocError::Create(lit->Begin(), "overflow int32");
    }
    return std::make_unique<mir::ConstantInt>(ty, n);
  } else if (ty->IsI64()) {
    return std::make_unique<mir::ConstantInt>(ty, n);
  } else if (ty->IsF32() || ty->IsF64()) {
    return std::make_unique<mir::ConstantFloat>(ty, n);
  } else {
    UNREACHABLE
  }
}

std::unique_ptr<mir::ConstantFloat> Lower::ParseFloatLit(
    std::unique_ptr<ast::Lit> lit) {
  std::string err;
  double n;
  if (!ParseFloat(lit->val, n, err)) {
    throw LocError::Create(lit->Begin(), err);
  }
  auto ty = ctx_.GetResult(lit).val;
  return std::make_unique<mir::ConstantFloat>(ty, n);
}

std::shared_ptr<mir::Var> Lower::LowerBinary(
    std::unique_ptr<ast::BinaryExpr> expr) {
  auto lhs = LowerExpr(std::move(expr->lhs));
  auto rhs = LowerExpr(std::move(expr->rhs));
  if (IsBinOp(expr->op->op)) {
    return builder_.CreateBinary(BinOp(expr->op->op), lhs, rhs);
  } else if (IsCmpOp(expr->op->op)) {
    return builder_.CreateCmp(CmpOp(expr->op->op), lhs, rhs);
  } else {
    UNREACHABLE
  }
}

std::shared_ptr<mir::Var> Lower::LowerCall(
    std::unique_ptr<ast::CallExpr> expr) {
  auto decl = ctx_.GetDecl(expr->ident);
  std::vector<std::shared_ptr<mir::Value>> args;
  while (!expr->args.empty()) {
    args.push_back(LowerExpr(expr->args.move_front()));
  }
  return builder_.CreateCall(decl, std::move(args));
}

std::shared_ptr<mir::Var> Lower::LowerUnary(
    std::unique_ptr<ast::UnaryExpr> unary) {
  auto ty = ctx_.GetResult(unary).val;
  auto expr = LowerExpr(std::move(unary->expr));
  return builder_.CreateUnary(UnaryOp(unary->op->kind), expr);
}

std::shared_ptr<mir::Var> Lower::LowerArray(
    std::unique_ptr<ast::ArrayExpr> array) {
  auto type = std::dynamic_pointer_cast<ArrayTy>(ctx_.GetResult(array).val);
  auto alloc_var = builder_.CreateVar(type, true);
  auto idx = 0;
  while (!array->exprs.empty()) {
    auto expr = array->exprs.move_front();
    auto res = LowerExpr(std::move(expr));

    auto idx_var = builder_.CreateVar(ToPtr(type->elem), false);
    builder_.current_bb->InsertInst(
        std::make_shared<mir::GepInst>(idx_var, alloc_var, idx++));
    builder_.CreateAssign(idx_var, res);
  }
  return alloc_var;
}

std::shared_ptr<mir::Var> Lower::LowerIf(std::unique_ptr<ast::If> if_stmt) {
  bool has_else = if_stmt->HasElse();

  auto stmt_res = ctx_.GetResult(if_stmt);
  std::shared_ptr<mir::Var> lval = nullptr;
  if (stmt_res.IsExpr()) {
    lval = builder_.CreateVar(stmt_res.val, true);
  }

  auto cond_expr = LowerExpr(std::move(if_stmt->cond));
  auto then_bb = builder_.CreateBB();

  if (!has_else) {
    // no else
    auto end_bb = builder_.CreateBB(then_bb);
    auto cond = builder_.CreateCond(cond_expr, then_bb, end_bb);
    builder_.SetInsertBB(cond->then_bb);
    auto block_res = ctx_.GetResult(if_stmt->block);
    auto then_result = LowerBlock(std::move(if_stmt->block));
    if (!block_res.IsRet()) {
      builder_.CreateGoto(end_bb);
    }
    builder_.SetInsertBB(end_bb);
    return nullptr;
  }

  if (stmt_res.IsRet()) {
    auto else_bb = builder_.CreateBB(then_bb);
    auto cond = builder_.CreateCond(cond_expr, then_bb, else_bb);
    builder_.SetInsertBB(then_bb);
    LowerBlock(std::move(if_stmt->block));

    builder_.SetInsertBB(else_bb);
    if (if_stmt->IsElseIf()) {
      LowerIf(unique_cast<ast::If>(std::move(if_stmt->els)));
    } else {
      LowerBlock(unique_cast<ast::Block>(std::move(if_stmt->els)));
    }
    return nullptr;
  }

  auto else_bb = builder_.CreateBB(then_bb);
  auto end_bb = builder_.CreateBB(else_bb);
  auto cond = builder_.CreateCond(cond_expr, then_bb, else_bb);
  builder_.SetInsertBB(then_bb);
  auto block_res = ctx_.GetResult(if_stmt->block);
  auto block_val = LowerBlock(std::move(if_stmt->block));
  if (!block_res.IsRet()) {
    if (lval && block_val) {
      builder_.CreateAssign(lval, block_val);
    }
    builder_.CreateGoto(end_bb);
  }

  builder_.SetInsertBB(else_bb);
  auto else_res = ctx_.GetResult(if_stmt->els);
  std::shared_ptr<mir::Value> else_val;
  if (if_stmt->IsElseIf()) {
    else_val = LowerIf(unique_cast<ast::If>(std::move(if_stmt->els)));
  } else {
    else_val = LowerBlock(unique_cast<ast::Block>(std::move(if_stmt->els)));
  }
  if (!else_res.IsRet()) {
    if (lval && else_val) {
      builder_.CreateAssign(lval, else_val);
    }
    builder_.CreateGoto(end_bb);
  }

  builder_.SetInsertBB(end_bb);

  return lval;
}

std::shared_ptr<mir::Value> Lower::LowerBlock(
    std::unique_ptr<ast::Block> block) {
  std::cout << "LowerBLOCK " << block.get() << std::endl;
  std::shared_ptr<mir::Value> val;
  while (!block->stmts.empty()) {
    auto stmt = block->stmts.move_front();
    val = LowerStmt(std::move(stmt));
  }
  std::cout << "END LowerBLOCK " << block.get() << std::endl;
  return val;
}

}  // namespace felis