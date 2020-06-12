
#include "lower.h"

#include <sstream>

#include "check/parse.h"
#include "check/type_checker.h"
#include "error/error.h"
#include "macro.h"

namespace felis {

// namespace {
//
// bool IsBinOp(ast::BinaryOp::Kind op) {
//  switch (op) {
//    case ast::BinaryOp::Kind::ADD:
//    case ast::BinaryOp::Kind::SUB:
//    case ast::BinaryOp::Kind::MUL:
//    case ast::BinaryOp::Kind::DIV:
//    case ast::BinaryOp::Kind::MOD:
//      return true;
//    default:
//      return false;
//  }
//}
//
// bool IsCmpOp(ast::BinaryOp::Kind op) {
//  switch (op) {
//    case ast::BinaryOp::Kind::EQEQ:
//    case ast::BinaryOp::Kind::NEQ:
//    case ast::BinaryOp::Kind::LT:
//    case ast::BinaryOp::Kind::LE:
//    case ast::BinaryOp::Kind::GT:
//    case ast::BinaryOp::Kind::GE:
//      return true;
//    default:
//      return false;
//  }
//}
//
// mir::BinaryInst::Op BinOp(ast::BinaryOp::Kind op) {
//  switch (op) {
//    case ast::BinaryOp::Kind::ADD:
//      return mir::BinaryInst::Op::ADD;
//    case ast::BinaryOp::Kind::SUB:
//      return mir::BinaryInst::Op::SUB;
//    case ast::BinaryOp::Kind::MUL:
//      return mir::BinaryInst::Op::MUL;
//    case ast::BinaryOp::Kind::DIV:
//      return mir::BinaryInst::Op::DIV;
//    case ast::BinaryOp::Kind::MOD:
//      return mir::BinaryInst::Op::MOD;
//    default:
//      UNREACHABLE
//  }
//}
//
// mir::CmpInst::Op CmpOp(ast::BinaryOp::Kind op) {
//  switch (op) {
//    case ast::BinaryOp::Kind::EQEQ:
//      return mir::CmpInst::Op::EQEQ;
//    case ast::BinaryOp::Kind::NEQ:
//      return mir::CmpInst::Op::NEQ;
//    case ast::BinaryOp::Kind::LT:
//      return mir::CmpInst::Op::LT;
//    case ast::BinaryOp::Kind::LE:
//      return mir::CmpInst::Op::LE;
//    case ast::BinaryOp::Kind::GT:
//      return mir::CmpInst::Op::GT;
//    case ast::BinaryOp::Kind::GE:
//      return mir::CmpInst::Op::GE;
//    default:
//      UNREACHABLE
//  }
//}
//
// mir::UnaryInst::Op UnaryOp(ast::UnaryOp::Kind op) {
//  switch (op) {
//    case ast::UnaryOp::NEG:
//      return mir::UnaryInst::Op::NEG;
//    case ast::UnaryOp::NOT:
//      return mir::UnaryInst::Op::NOT;
//  }
//}
//
//}  // namespace
//
// std::unique_ptr<mir::File> Lowering(std::unique_ptr<ast::File> file,
//                                    bool is_32bit) {
//  TypeCheckCtx ctx(is_32bit);
//  TypeChecker(ctx).Check(file);
//  ctx.FinalizeType();
//
//  auto mir_file = std::make_unique<mir::File>();
//  Lower(ctx, mir_file).Lowering(std::move(file));
//  return std::move(mir_file);
//}
//
// void Lower::Lowering(std::unique_ptr<ast::File> file) {
//  while (!file->externs.empty()) {
//    auto ext = file->externs.move_front();
//    auto decl = ctx_.GetDecl(ext->proto->name);
//    builder_.CreateFunc(decl);
//  }
//
//  for (auto &fn_decl : file->fn_decls) {
//    auto decl = ctx_.GetDecl(fn_decl->proto->name);
//    auto func =
//        std::dynamic_pointer_cast<mir::Function>(builder_.CreateFunc(decl));
//    builder_.SetInsertBB(func->entry_bb);
//
//    for (auto &arg : fn_decl->proto->args->list) {
//      auto decl = ctx_.GetDecl(arg->name);
//      auto arg_value = builder_.CreateResult(ctx_.ResolvedType(decl->type));
//      func->args.push_back(arg_value);
//
//      auto arg_alloc = builder_.CreateAlloc(decl->type);
//      builder_.SetDeclValue(decl, arg_alloc);
//      builder_.CreateAssign(arg_alloc, arg_value);
//    }
//  }
//
//  while (!file->fn_decls.empty()) {
//    auto fn_decl = file->fn_decls.move_front();
//    auto decl = ctx_.GetDecl(fn_decl->proto->name);
//    auto function = builder_.GetDeclFunc<mir::Function>(decl);
//    builder_.SetInsertBB(function->entry_bb);
//
//    auto block_res = ctx_.GetResult(fn_decl->block);
//    auto val = LowerBlock(std::move(fn_decl->block));
//    if (!block_res.IsRet()) {
//      if (function->type->ret->IsVoid()) {
//        builder_.CreateRet(nullptr);
//      } else {
//        builder_.CreateRet(val);
//      }
//    }
//  }
//}
//
// std::shared_ptr<mir::Value> Lower::LowerStmt(std::unique_ptr<ast::Stmt> stmt)
// {
//  switch (stmt->StmtKind()) {
//    case ast::Stmt::Kind::EXPR:
//      return LowerExpr(unique_cast<ast::Expr>(std::move(stmt)));
//    case ast::Stmt::Kind::RET:
//      LowerRet(unique_cast<ast::RetStmt>(std::move(stmt)));
//      break;
//    case ast::Stmt::Kind::VAR_DECL:
//      LowerVarDecl(unique_cast<ast::VarDeclStmt>(std::move(stmt)));
//      break;
//    case ast::Stmt::Kind::ASSIGN:
//      LowerAssign(unique_cast<ast::AssignStmt>(std::move(stmt)));
//      break;
//  }
//  return nullptr;
//}
//
// void Lower::LowerRet(std::unique_ptr<ast::RetStmt> stmt) {
//  if (stmt->expr) {
//    auto val = LowerExpr(std::move(stmt->expr));
//    builder_.CreateRet(val);
//  } else {
//    builder_.CreateRet();
//  }
//}
//
// void Lower::LowerVarDecl(std::unique_ptr<ast::VarDeclStmt> stmt) {
//  auto decl = ctx_.GetDecl(stmt->name);
//  auto val = LowerExpr(std::move(stmt->expr));
//
//  //  if (val->IsRValue() && *ToPtr(decl->type) == *val->type) {
//  //    // array
//  //    builder_.SetDeclValue(decl, val);
//  //    return;
//  //  }
//
//  auto var = builder_.CreateAlloc(decl->type);
//  builder_.SetDeclValue(decl, var);
//  builder_.CreateAssign(var, val);
//}
//
// void Lower::LowerAssign(std::unique_ptr<ast::AssignStmt> stmt) {
//  auto decl = ctx_.GetDecl(stmt->name);
//  auto value = builder_.GetDeclValue(decl);
//  auto expr_value = LowerExpr(std::move(stmt->expr));
//  builder_.CreateAssign(value, expr_value);
//}
//
// std::shared_ptr<mir::Value> Lower::LowerExpr(std::unique_ptr<ast::Expr> expr)
// {
//  switch (expr->ExprKind()) {
//    case ast::Expr::Kind::IDENT:
//      return builder_.GetDeclValue(
//          ctx_.GetDecl(unique_cast<ast::Ident>(std::move(expr))));
//    case ast::Expr::Kind::LIT:
//      return LowerLit(unique_cast<ast::Lit>(std::move(expr)));
//    case ast::Expr::Kind::BINARY:
//      return LowerBinary(unique_cast<ast::BinaryExpr>(std::move(expr)));
//    case ast::Expr::Kind::CALL:
//      return LowerCall(unique_cast<ast::CallExpr>(std::move(expr)));
//    case ast::Expr::Kind::UNARY:
//      return LowerUnary(unique_cast<ast::UnaryExpr>(std::move(expr)));
//    case ast::Expr::Kind::ARRAY:
//      return LowerArray(unique_cast<ast::ArrayExpr>(std::move(expr)));
//    case ast::Expr::Kind::BLOCK:
//      return LowerBlock(unique_cast<ast::Block>(std::move(expr)));
//    case ast::Expr::Kind::IF:
//      return LowerIf(unique_cast<ast::If>(std::move(expr)));
//  }
//}
//
// std::shared_ptr<mir::Value> Lower::LowerLit(std::unique_ptr<ast::Lit> lit) {
//  switch (lit->LitKind()) {
//    case ast::Lit::Kind::CHAR: {
//      auto ty = ctx_.GetResult(lit).type;
//
//      std::stringstream ss(lit->val);
//      rune r;
//      ss >> r;
//
//      if (ty->IsI32() || ty->IsI64()) {
//        return builder_.CreateConstInt(ty, r);
//      } else if (ty->IsF32() || ty->IsF64()) {
//        return builder_.CreateConstFloat(ty, r);
//      } else {
//        UNREACHABLE
//      }
//    } break;
//    case ast::Lit::Kind::INT: {
//      return ParseIntLit(std::move(lit));
//    } break;
//    case ast::Lit::Kind::FLOAT: {
//      return ParseFloatLit(std::move(lit));
//    } break;
//    case ast::Lit::Kind::BOOL: {
//      return builder_.CreateConstBool(lit->val == "true");
//    } break;
//    case ast::Lit::Kind::STRING: {
//      return builder_.CreateConstString(lit->val);
//    } break;
//  }
//}
//
// std::shared_ptr<mir::Value> Lower::ParseIntLit(std::unique_ptr<ast::Lit> lit)
// {
//  int64_t n;
//  std::string err;
//  if (!ParseInt(lit->val, n, err)) {
//    throw LocError::Create(lit->Begin(), err);
//  }
//  auto ty = ctx_.GetResult(lit).type;
//  if (ty->IsI8()) {
//    if (n < INT8_MIN || n > INT8_MAX) {
//      throw LocError::Create(lit->Begin(), "overflow int8");
//    }
//    return builder_.CreateConstInt(ty, n);
//  } else if (ty->IsI16()) {
//    if (n < INT16_MIN || n > INT16_MAX) {
//      throw LocError::Create(lit->Begin(), "overflow int16");
//    }
//    return builder_.CreateConstInt(ty, n);
//  } else if (ty->IsI32()) {
//    if (n < INT32_MIN || n > INT32_MAX) {
//      throw LocError::Create(lit->Begin(), "overflow int32");
//    }
//    return builder_.CreateConstInt(ty, n);
//  } else if (ty->IsI64()) {
//    return builder_.CreateConstInt(ty, n);
//  } else if (ty->IsF32() || ty->IsF64()) {
//    return builder_.CreateConstFloat(ty, n);
//  } else {
//    UNREACHABLE
//  }
//}
//
// std::shared_ptr<mir::Value> Lower::ParseFloatLit(
//    std::unique_ptr<ast::Lit> lit) {
//  std::string err;
//  double n;
//  if (!ParseFloat(lit->val, n, err)) {
//    throw LocError::Create(lit->Begin(), err);
//  }
//  auto ty = ctx_.GetResult(lit).type;
//  return builder_.CreateConstFloat(ty, n);
//}
//
// std::shared_ptr<mir::Value> Lower::LowerBinary(
//    std::unique_ptr<ast::BinaryExpr> expr) {
//  auto ty = ctx_.GetResult(expr).type;
//  auto lhs = LowerExpr(std::move(expr->lhs));
//  auto rhs = LowerExpr(std::move(expr->rhs));
//  if (IsBinOp(expr->op->op)) {
//    return builder_.CreateBinary(ty, BinOp(expr->op->op), lhs, rhs);
//  } else if (IsCmpOp(expr->op->op)) {
//    return builder_.CreateCmp(CmpOp(expr->op->op), lhs, rhs);
//  } else {
//    UNREACHABLE
//  }
//}
//
// std::shared_ptr<mir::Value> Lower::LowerCall(
//    std::unique_ptr<ast::CallExpr> expr) {
//  auto decl = ctx_.GetDecl(expr->ident);
//  std::vector<std::shared_ptr<mir::Value>> args;
//  while (!expr->args.empty()) {
//    args.push_back(LowerExpr(expr->args.move_front()));
//  }
//  return builder_.CreateCall(decl, std::move(args));
//}
//
// std::shared_ptr<mir::Value> Lower::LowerUnary(
//    std::unique_ptr<ast::UnaryExpr> unary) {
//  auto ty = ctx_.GetResult(unary).type;
//  auto expr = LowerExpr(std::move(unary->expr));
//  return builder_.CreateUnary(ty, UnaryOp(unary->op->kind), expr);
//}
//
// std::shared_ptr<mir::Value> Lower::LowerArray(
//    std::unique_ptr<ast::ArrayExpr> array) {
//  auto type = std::dynamic_pointer_cast<ArrayTy>(ctx_.GetResult(array).type);
//  auto var = builder_.CreateAlloc(type);
//
//  std::vector<std::shared_ptr<mir::Value>> values;
//  while (!array->exprs.empty()) {
//    auto expr = array->exprs.move_front();
//    values.push_back(LowerExpr(std::move(expr)));
//  }
//
//  builder_.Insert(std::make_shared<mir::ArrayInst>(var, values));
//  return var;
//}
//
// std::shared_ptr<mir::Value> Lower::LowerIf(std::unique_ptr<ast::If> if_stmt)
// {
//  bool has_else = if_stmt->HasElse();
//  auto stmt_res = ctx_.GetResult(if_stmt);
//
//  std::shared_ptr<mir::PhiInst> phi_inst = nullptr;
//  if (stmt_res.IsExpr()) {
//    phi_inst =
//        std::make_shared<mir::PhiInst>(builder_.CreateResult(stmt_res.type));
//  }
//
//  auto cond_expr = LowerExpr(std::move(if_stmt->cond));
//  auto then_bb = builder_.CreateBB();
//
//  if (!has_else) {
//    // no else
//    auto end_bb = builder_.CreateBB(then_bb);
//    auto cond = builder_.CreateCond(cond_expr, then_bb, end_bb);
//    builder_.SetInsertBB(cond->then_bb);
//    auto block_res = ctx_.GetResult(if_stmt->block);
//    auto then_result = LowerBlock(std::move(if_stmt->block));
//    if (!block_res.IsRet()) {
//      builder_.CreateGoto(end_bb);
//    }
//    builder_.SetInsertBB(end_bb);
//    return nullptr;
//  }
//
//  if (stmt_res.IsRet()) {
//    // terminating
//    auto else_bb = builder_.CreateBB(then_bb);
//    auto cond = builder_.CreateCond(cond_expr, then_bb, else_bb);
//    builder_.SetInsertBB(then_bb);
//    LowerBlock(std::move(if_stmt->block));
//
//    builder_.SetInsertBB(else_bb);
//    if (if_stmt->IsElseIf()) {
//      LowerIf(unique_cast<ast::If>(std::move(if_stmt->els)));
//    } else {
//      LowerBlock(unique_cast<ast::Block>(std::move(if_stmt->els)));
//    }
//    return nullptr;
//  }
//
//  auto else_bb = builder_.CreateBB(then_bb);
//  auto end_bb = builder_.CreateBB(else_bb);
//  auto cond = builder_.CreateCond(cond_expr, then_bb, else_bb);
//  builder_.SetInsertBB(then_bb);
//  auto block_res = ctx_.GetResult(if_stmt->block);
//  auto block_val = LowerBlock(std::move(if_stmt->block));
//  if (!block_res.IsRet()) {
//    // needs goto
//    if (phi_inst && block_val) {
//      // phi
//      phi_inst->nodes.push_back(std::make_pair(block_val, then_bb));
//    }
//    builder_.CreateGoto(end_bb);
//  }
//
//  builder_.SetInsertBB(else_bb);
//  auto else_res = ctx_.GetResult(if_stmt->els);
//  std::shared_ptr<mir::Value> else_val;
//  if (if_stmt->IsElseIf()) {
//    else_val = LowerIf(unique_cast<ast::If>(std::move(if_stmt->els)));
//  } else {
//    else_val = LowerBlock(unique_cast<ast::Block>(std::move(if_stmt->els)));
//  }
//  if (!else_res.IsRet()) {
//    if (phi_inst && else_val) {
//      phi_inst->nodes.push_back(std::make_pair(else_val, else_bb));
//    }
//    builder_.CreateGoto(end_bb);
//  }
//
//  builder_.SetInsertBB(end_bb);
//  builder_.Insert(phi_inst);
//  return phi_inst->result;
//  //  return lval;
//}
//
// std::shared_ptr<mir::Value> Lower::LowerBlock(
//    std::unique_ptr<ast::Block> block) {
//  std::shared_ptr<mir::Value> val;
//  while (!block->stmts.empty()) {
//    auto stmt = block->stmts.move_front();
//    val = LowerStmt(std::move(stmt));
//  }
//  return val;
//}

}  // namespace felis
