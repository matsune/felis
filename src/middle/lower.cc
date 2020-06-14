
#include "lower.h"

#include <sstream>

#include "check/parse.h"
#include "check/type_checker.h"
#include "error/error.h"
#include "macro.h"

namespace felis {

namespace {

bool IsBinOp(ast::BinaryOp::Kind op) {
  switch (op) {
    case ast::BinaryOp::Kind::ADD:
    case ast::BinaryOp::Kind::SUB:
    case ast::BinaryOp::Kind::MUL:
    case ast::BinaryOp::Kind::DIV:
    case ast::BinaryOp::Kind::MOD:
      return true;
    default:
      return false;
  }
}

bool IsCmpOp(ast::BinaryOp::Kind op) {
  switch (op) {
    case ast::BinaryOp::Kind::EQEQ:
    case ast::BinaryOp::Kind::NEQ:
    case ast::BinaryOp::Kind::LT:
    case ast::BinaryOp::Kind::LE:
    case ast::BinaryOp::Kind::GT:
    case ast::BinaryOp::Kind::GE:
      return true;
    default:
      return false;
  }
}

mir::BinaryInst::Op BinOp(ast::BinaryOp::Kind op) {
  switch (op) {
    case ast::BinaryOp::Kind::ADD:
      return mir::BinaryInst::Op::ADD;
    case ast::BinaryOp::Kind::SUB:
      return mir::BinaryInst::Op::SUB;
    case ast::BinaryOp::Kind::MUL:
      return mir::BinaryInst::Op::MUL;
    case ast::BinaryOp::Kind::DIV:
      return mir::BinaryInst::Op::DIV;
    case ast::BinaryOp::Kind::MOD:
      return mir::BinaryInst::Op::MOD;
    default:
      UNREACHABLE
  }
}

mir::CmpInst::Op CmpOp(ast::BinaryOp::Kind op) {
  switch (op) {
    case ast::BinaryOp::Kind::EQEQ:
      return mir::CmpInst::Op::EQEQ;
    case ast::BinaryOp::Kind::NEQ:
      return mir::CmpInst::Op::NEQ;
    case ast::BinaryOp::Kind::LT:
      return mir::CmpInst::Op::LT;
    case ast::BinaryOp::Kind::LE:
      return mir::CmpInst::Op::LE;
    case ast::BinaryOp::Kind::GT:
      return mir::CmpInst::Op::GT;
    case ast::BinaryOp::Kind::GE:
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
  ctx.ResolveTypes();

  auto mir_file = std::make_unique<mir::File>();
  Lower(ctx, mir_file).Lowering(std::move(file));
  return std::move(mir_file);
}

void Lower::Lowering(std::unique_ptr<ast::File> file) {
  for (auto ext : file->externs) {
    auto decl = ctx_.GetDecl(ext->proto->name);
    builder_.CreateFunc(decl);
  }

  for (auto func : file->funcs) {
    auto decl = ctx_.GetDecl(func->proto->name);
    auto fn =
        std::dynamic_pointer_cast<mir::Function>(builder_.CreateFunc(decl));
    builder_.SetInsertBB(fn->entry_bb);

    for (auto& arg : func->proto->args->list) {
      auto decl = ctx_.GetDecl(arg->name);
      auto arg_value = builder_.CreateResult(decl->type);
      fn->args.push_back(arg_value);

      auto arg_alloc = builder_.CreateAlloc(decl->type);
      builder_.SetDeclValue(decl, arg_alloc);
      builder_.CreateAssign(arg_alloc, arg_value);
    }
  }

  for (auto func : file->funcs) {
    auto decl = ctx_.GetDecl(func->proto->name);
    auto function = builder_.GetDeclFunc<mir::Function>(decl);
    builder_.SetInsertBB(function->entry_bb);

    auto block_res = ctx_.GetResult(func->block);
    auto val = LowerBlock(std::move(func->block));
    if (!block_res.IsRet()) {
      if (function->type->GetRet()->IsVoid()) {
        builder_.CreateRet(nullptr);
      } else {
        builder_.CreateRet(val);
      }
    }
  }
}

std::shared_ptr<mir::Value> Lower::LowerStmt(ast::AstNode* stmt) {
  if (auto ret = node_cast_ornull<ast::RetStmt>(stmt)) {
    LowerRet(ret);
  } else if (auto var_decl = node_cast_ornull<ast::VarDeclStmt>(stmt)) {
    LowerVarDecl(var_decl);
  } else if (auto assign = node_cast_ornull<ast::AssignStmt>(stmt)) {
    LowerAssign(assign);
  } else {
    return LowerExpr(stmt);
  }
  return nullptr;
}

void Lower::LowerRet(ast::RetStmt* stmt) {
  if (stmt->expr) {
    builder_.CreateRet(LowerExpr(stmt->expr));
  } else {
    builder_.CreateRet();
  }
}

void Lower::LowerVarDecl(ast::VarDeclStmt* stmt) {
  auto decl = ctx_.GetDecl(stmt->name);
  auto val = LowerExpr(std::move(stmt->expr));

  //  if (val->IsRValue() && *ToPtr(decl->type) == *val->type) {
  //    // array
  //    builder_.SetDeclValue(decl, val);
  //    return;
  //  }

  auto var = builder_.CreateAlloc(decl->type);
  builder_.SetDeclValue(decl, var);
  builder_.CreateAssign(var, val);
}

void Lower::LowerAssign(ast::AssignStmt* stmt) {
  if (auto ident = node_cast_ornull<ast::Ident>(stmt)) {
    auto decl = ctx_.GetDecl(ident);
    auto value = builder_.GetDeclValue(decl);
    auto expr_value = LowerExpr(stmt->expr);
    builder_.CreateAssign(value, expr_value);
  } else {
    UNIMPLEMENTED
  }
}

std::shared_ptr<mir::Value> Lower::LowerExpr(ast::AstNode* expr) {
  if (auto ident = node_cast_ornull<ast::Ident>(expr)) {
    return builder_.GetDeclValue(ctx_.GetDecl(ident));
  } else if (auto lit = node_cast_ornull<ast::Literal>(expr)) {
    return LowerLit(lit);
  } else if (auto binary = node_cast_ornull<ast::Binary>(expr)) {
    return LowerBinary(binary);
  } else if (auto call = node_cast_ornull<ast::Call>(expr)) {
    return LowerCall(call);
  } else if (auto unary = node_cast_ornull<ast::Unary>(expr)) {
    return LowerUnary(unary);
  } else if (auto array = node_cast_ornull<ast::Array>(expr)) {
    return LowerArray(array);
  } else if (auto block = node_cast_ornull<ast::Block>(expr)) {
    return LowerBlock(block);
  } else if (auto if_stmt = node_cast_ornull<ast::If>(expr)) {
    return LowerIf(if_stmt);
  } else {
    UNREACHABLE
  }
}

std::shared_ptr<mir::Value> Lower::LowerLit(ast::Literal* lit) {
  switch (lit->kind) {
    case ast::Literal::Kind::CHAR: {
      auto ty = ctx_.GetResult(lit).type;

      std::stringstream ss(lit->val);
      rune r;
      ss >> r;

      if (ty->IsI32() || ty->IsI64()) {
        return builder_.CreateConstInt(ty, r);
      } else if (ty->IsF32() || ty->IsF64()) {
        return builder_.CreateConstFloat(ty, r);
      } else {
        UNREACHABLE
      }
    } break;
    case ast::Literal::Kind::INT: {
      return ParseIntLit(std::move(lit));
    } break;
    case ast::Literal::Kind::FLOAT: {
      return ParseFloatLit(std::move(lit));
    } break;
    case ast::Literal::Kind::BOOL: {
      return builder_.CreateConstBool(lit->val == "true");
    } break;
    case ast::Literal::Kind::STRING: {
      return builder_.CreateConstString(lit->val);
    } break;
  }
}

std::shared_ptr<mir::Value> Lower::ParseIntLit(ast::Literal* lit) {
  int64_t n;
  std::string err;
  if (!ParseInt(lit->val, n, err)) {
    throw LocError::Create(lit->begin, err);
  }
  auto ty = ctx_.GetResult(lit).type;
  if (ty->IsI8()) {
    if (n < INT8_MIN || n > INT8_MAX) {
      throw LocError::Create(lit->begin, "overflow int8");
    }
    return builder_.CreateConstInt(ty, n);
  } else if (ty->IsI16()) {
    if (n < INT16_MIN || n > INT16_MAX) {
      throw LocError::Create(lit->begin, "overflow int16");
    }
    return builder_.CreateConstInt(ty, n);
  } else if (ty->IsI32()) {
    if (n < INT32_MIN || n > INT32_MAX) {
      throw LocError::Create(lit->begin, "overflow int32");
    }
    return builder_.CreateConstInt(ty, n);
  } else if (ty->IsI64()) {
    return builder_.CreateConstInt(ty, n);
  } else if (ty->IsF32() || ty->IsF64()) {
    return builder_.CreateConstFloat(ty, n);
  } else {
    std::cout << ty.get() << std::endl;
    UNREACHABLE
  }
}

std::shared_ptr<mir::Value> Lower::ParseFloatLit(ast::Literal* lit) {
  std::string err;
  double n;
  if (!ParseFloat(lit->val, n, err)) {
    throw LocError::Create(lit->begin, err);
  }
  auto ty = ctx_.GetResult(lit).type;
  return builder_.CreateConstFloat(ty, n);
}

std::shared_ptr<mir::Value> Lower::LowerBinary(ast::Binary* expr) {
  auto ty = ctx_.GetResult(expr).type;
  auto lhs = LowerExpr(expr->lhs);
  auto rhs = LowerExpr(expr->rhs);
  if (IsBinOp(expr->op->kind)) {
    return builder_.CreateBinary(ty, BinOp(expr->op->kind), lhs, rhs);
  } else if (IsCmpOp(expr->op->kind)) {
    return builder_.CreateCmp(CmpOp(expr->op->kind), lhs, rhs);
  } else {
    UNREACHABLE
  }
}

std::shared_ptr<mir::Value> Lower::LowerCall(ast::Call* expr) {
  auto decl = ctx_.GetDecl(expr->ident);
  std::vector<std::shared_ptr<mir::Value>> args;
  for (auto arg : expr->args) {
    args.push_back(LowerExpr(arg));
  }
  return builder_.CreateCall(decl, args);
}

std::shared_ptr<mir::Value> Lower::LowerUnary(ast::Unary* unary) {
  auto ty = ctx_.GetResult(unary).type;
  auto expr = LowerExpr(unary->expr);
  return builder_.CreateUnary(ty, UnaryOp(unary->op->kind), expr);
}

std::shared_ptr<mir::Value> Lower::LowerArray(ast::Array* array) {
  auto type = ctx_.GetResult(array).type;
  auto var = builder_.CreateAlloc(type);

  std::vector<std::shared_ptr<mir::Value>> values;
  for (auto expr : array->exprs) {
    values.push_back(LowerExpr(expr));
  }

  builder_.Insert(std::make_shared<mir::ArrayInst>(var, values));
  return var;
}

std::shared_ptr<mir::Value> Lower::LowerIf(ast::If* if_stmt) {
  bool has_else = if_stmt->HasElse();
  auto stmt_res = ctx_.GetResult(if_stmt);

  std::shared_ptr<mir::PhiInst> phi_inst = nullptr;
  if (stmt_res.IsExpr()) {
    phi_inst =
        std::make_shared<mir::PhiInst>(builder_.CreateResult(stmt_res.type));
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
    // terminating
    auto else_bb = builder_.CreateBB(then_bb);
    auto cond = builder_.CreateCond(cond_expr, then_bb, else_bb);
    builder_.SetInsertBB(then_bb);
    LowerBlock(std::move(if_stmt->block));

    builder_.SetInsertBB(else_bb);
    if (if_stmt->IsElseIf()) {
      LowerIf(node_cast<ast::If>(if_stmt->els));
    } else {
      LowerBlock(node_cast<ast::Block>(if_stmt->els));
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
    // needs goto
    if (phi_inst && block_val) {
      // phi
      phi_inst->nodes.push_back(std::make_pair(block_val, then_bb));
    }
    builder_.CreateGoto(end_bb);
  }

  builder_.SetInsertBB(else_bb);
  auto else_res = ctx_.GetResult(if_stmt->els);
  std::shared_ptr<mir::Value> else_val;
  if (if_stmt->IsElseIf()) {
    else_val = LowerIf(node_cast<ast::If>(if_stmt->els));
  } else {
    else_val = LowerBlock(node_cast<ast::Block>(if_stmt->els));
  }
  if (!else_res.IsRet()) {
    if (phi_inst && else_val) {
      phi_inst->nodes.push_back(std::make_pair(else_val, else_bb));
    }
    builder_.CreateGoto(end_bb);
  }

  builder_.SetInsertBB(end_bb);
  if (phi_inst) {
    builder_.Insert(phi_inst);
    return phi_inst->result;
  }
  return nullptr;
}

std::shared_ptr<mir::Value> Lower::LowerBlock(ast::Block* block) {
  std::shared_ptr<mir::Value> val;
  for (auto stmt : block->stmts) {
    val = LowerStmt(stmt);
  }
  return val;
}

}  // namespace felis
