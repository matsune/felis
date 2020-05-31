
#include "lower.h"

#include <sstream>

#include "check/parse.h"
#include "error/error.h"

namespace felis {

namespace {

std::shared_ptr<Type> FinalType(std::shared_ptr<Type> ty, bool is_32bit) {
  auto underlying_ty = Underlying(ty);
  if (underlying_ty->IsFixed()) {
    if (underlying_ty->IsArray()) {
      auto array_ty = std::dynamic_pointer_cast<ArrayType>(underlying_ty);
      auto elem = FinalType(array_ty->elem, is_32bit);
      return std::make_shared<ArrayType>(elem, array_ty->size);
    }
    return underlying_ty;
  } else if (underlying_ty->IsUntyped()) {
    auto untyped = std::dynamic_pointer_cast<Untyped>(underlying_ty);
    if (untyped->IsUntypedInt()) {
      return is_32bit ? kTypeI32 : kTypeI64;
    } else if (untyped->IsUntypedFloat()) {
      return kTypeF32;
    }
  }
  std::cout << "[unreachabel] underlying " << ToString(underlying_ty)
            << std::endl;
  UNREACHABLE
}

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

mir::UnaryInst::Op UnaryOp(ast::UnaryOp::Op op) {
  switch (op) {
    case ast::UnaryOp::Op::NEG:
      return mir::UnaryInst::Op::NEG;
    case ast::UnaryOp::Op::NOT:
      return mir::UnaryInst::Op::NOT;
  }
}

}  // namespace

std::unique_ptr<mir::File> Lowering(std::unique_ptr<ast::File> file,
                                    bool is_32bit) {
  IdentDeclMap ident_decl_map;
  ExprTypeMap expr_type_map;

  TypeChecker type_ck(is_32bit, ident_decl_map, expr_type_map);
  type_ck.Check(file);

  // finalize types
  std::cout << "---------------" << std::endl;
  for (auto &it : ident_decl_map) {
    it.second->type = FinalType(it.second->type, is_32bit);
    std::cout << "ident: " << it.first << " decl: " << ToString(it.second)
              << std::endl;
  }
  std::cout << "---------------" << std::endl;
  for (auto &it : expr_type_map) {
    it.second = FinalType(it.second, is_32bit);
    std::cout << "expr: " << it.first << " type: " << ToString(it.second)
              << std::endl;
  }
  std::cout << "---------------" << std::endl;

  auto mir_file = std::make_unique<mir::File>();
  Lower(ident_decl_map, expr_type_map, mir_file).Lowering(std::move(file));
  return std::move(mir_file);
}

void Lower::Lowering(std::unique_ptr<ast::File> file) {
  std::cout << "CreateFn ext" << std::endl;
  while (!file->externs.empty()) {
    auto ext = file->externs.move_front();
    auto decl = GetDecl(ext->proto->name);
    builder_.CreateFunc(decl);
  }

  std::cout << "CreateFn fn" << std::endl;
  for (auto &fn_decl : file->fn_decls) {
    auto decl = GetDecl(fn_decl->proto->name);
    auto func =
        std::dynamic_pointer_cast<mir::Function>(builder_.CreateFunc(decl));
    builder_.SetInsertBB(func->entry_bb);

    auto ret_ty = decl->AsFuncType()->ret;
    std::shared_ptr<mir::LValue> ret_val = nullptr;
    if (!ret_ty->IsVoid()) {
      ret_val = builder_.CreateAlloc(ret_ty);
    }
    func->ret = ret_val;

    for (auto &arg : fn_decl->proto->args->list) {
      auto decl = GetDecl(arg->name);
      func->args.push_back(builder_.CreateAlloc(decl));
    }
  }

  std::cout << "Lower Fn" << std::endl;
  while (!file->fn_decls.empty()) {
    auto fn_decl = file->fn_decls.move_front();
    auto decl = GetDecl(fn_decl->proto->name);
    auto function =
        std::dynamic_pointer_cast<mir::Function>(builder_.GetFunction(decl));
    builder_.SetInsertBB(function->entry_bb);

    LowerBlock(std::move(fn_decl->block), nullptr);
  }
  std::cout << "End Lowering" << std::endl;
}

std::shared_ptr<mir::RValue> Lower::LowerStmt(std::unique_ptr<ast::Stmt> stmt,
                                              std::shared_ptr<mir::BB> end_bb) {
  std::cout << "LowerStmt " << ToString(stmt->StmtKind()) << std::endl;
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      return LowerExpr(unique_cast<ast::Expr>(std::move(stmt)), end_bb);
    case ast::Stmt::Kind::ASSIGN: {
      auto assign = unique_cast<ast::AssignStmt>(std::move(stmt));
      auto decl = GetDecl(assign->name);
      auto lval = builder_.GetVar(decl);
      auto rval = LowerExpr(std::move(assign->expr));
      builder_.CreateStore(lval, rval);
    } break;
    case ast::Stmt::Kind::VAR_DECL: {
      auto var_decl = unique_cast<ast::VarDeclStmt>(std::move(stmt));
      auto decl = GetDecl(var_decl->name);
      auto lval = builder_.CreateAlloc(decl);
      auto rval = LowerExpr(std::move(var_decl->expr));
      builder_.CreateStore(lval, rval);
    } break;
    case ast::Stmt::Kind::RET: {
      auto ret = unique_cast<ast::RetStmt>(std::move(stmt));
      if (ret->expr) {
        auto rval = LowerExpr(std::move(ret->expr));
        builder_.CreateStore(builder_.GetInsertBB()->parent.ret, rval);
        auto ret_val = builder_.CreateLoad(builder_.GetInsertBB()->parent.ret);
        builder_.CreateRet(ret_val);
      } else {
        builder_.CreateRet();
      }
    } break;
  }
  return nullptr;
}

std::shared_ptr<mir::RValue> Lower::LowerExpr(std::unique_ptr<ast::Expr> expr,
                                              std::shared_ptr<mir::BB> end_bb) {
  std::cout << "LowerExpr " << ToString(expr->ExprKind()) << std::endl;
  switch (expr->ExprKind()) {
    case ast::Expr::Kind::IDENT: {
      auto ident = unique_cast<ast::Ident>(std::move(expr));
      auto decl = GetDecl(ident);
      return builder_.CreateLoad(decl);
    } break;
    case ast::Expr::Kind::BINARY: {
      auto binary = unique_cast<ast::BinaryExpr>(std::move(expr));
      auto lhs = LowerExpr(std::move(binary->lhs));
      auto rhs = LowerExpr(std::move(binary->rhs));
      auto ty = GetType(binary);
      if (IsBinOp(binary->op->op)) {
        auto op = BinOp(binary->op->op);
        return builder_.CreateBinary(op, lhs, rhs);
      } else if (IsCmpOp(binary->op->op)) {
        auto op = CmpOp(binary->op->op);
        return builder_.CreateCmp(op, lhs, rhs);
      } else {
        UNREACHABLE
      }
    } break;
    case ast::Expr::Kind::LIT: {
      return LowerLit(unique_cast<ast::Lit>(std::move(expr)));
    } break;
    case ast::Expr::Kind::CALL: {
      auto call = unique_cast<ast::CallExpr>(std::move(expr));
      auto decl = GetDecl(call->ident);
      std::vector<std::shared_ptr<mir::RValue>> args;
      while (!call->args.empty()) {
        args.push_back(LowerExpr(call->args.move_front()));
      }
      return builder_.CreateCall(decl, std::move(args));
    } break;
    case ast::Expr::Kind::UNARY: {
      auto unary = unique_cast<ast::UnaryExpr>(std::move(expr));
      auto ty = GetType(unary);
      auto expr = LowerExpr(std::move(unary->expr));
      return builder_.CreateUnary(UnaryOp(unary->op->op), expr);
    } break;
    case ast::Expr::Kind::BLOCK: {
      return LowerBlock(unique_cast<ast::Block>(std::move(expr)), end_bb);
    } break;
    case ast::Expr::Kind::IF: {
      return LowerIf(unique_cast<ast::If>(std::move(expr)), end_bb);
    } break;
    case ast::Expr::Kind::ARRAY: {
      auto array = unique_cast<ast::ArrayExpr>(std::move(expr));
      auto type = GetType(array);
      std::vector<std::shared_ptr<mir::RValue>> values;
      while (!array->exprs.empty()) {
        auto expr = array->exprs.move_front();
        values.push_back(LowerExpr(std::move(expr)));
      }
      return builder_.CreateArray(type, values);

    } break;
  }
  UNREACHABLE
}

std::unique_ptr<mir::Constant> Lower::LowerLit(std::unique_ptr<ast::Lit> lit) {
  switch (lit->LitKind()) {
    case ast::Lit::Kind::CHAR: {
      auto ty = GetType(lit);

      std::stringstream ss(lit->val);
      rune r;
      ss >> r;

      if (ty->IsI32() || ty->IsI64()) {
        return std::make_unique<mir::ConstantInt>(ty, r);
      } else if (ty->IsF32() || ty->IsF64()) {
        return std::make_unique<mir::ConstantFloat>(ty, r);
      } else {
        UNREACHABLE
      }

    } break;
    case ast::Lit::Kind::INT:
      return ParseIntLit(std::move(lit));
    case ast::Lit::Kind::FLOAT: {
      return ParseFloatLit(std::move(lit));
    } break;
    case ast::Lit::Kind::BOOL: {
      auto ty = GetType(lit);
      return std::make_unique<mir::ConstantBool>(ty, lit->val == "true");
    } break;
    case ast::Lit::Kind::STRING: {
      auto ty = GetType(lit);
      return std::make_unique<mir::ConstantString>(ty, lit->val);
    } break;
  }
}

std::unique_ptr<mir::Constant> Lower::ParseIntLit(
    std::unique_ptr<ast::Lit> lit) {
  int64_t n;
  std::string err;
  if (!ParseInt(lit->val, n, err)) {
    throw LocError::Create(lit->Begin(), err);
  }

  auto ty = GetType(lit);
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
  auto ty = GetType(lit);
  assert(ty->IsFixedFloat());
  return std::make_unique<mir::ConstantFloat>(ty, n);
}

std::shared_ptr<mir::RValue> Lower::LowerIf(std::unique_ptr<ast::If> if_stmt,
                                            std::shared_ptr<mir::BB> end_bb) {
  bool has_else = if_stmt->HasElse();
  bool is_then_terminating = if_stmt->block->IsTerminating();

  // non void type if-stmt is used for expr
  std::shared_ptr<mir::LValue> lval = nullptr;
  auto ty = GetType(if_stmt);
  if (!ty->IsVoid()) {
    lval = builder_.CreateAlloc(ty);
  }

  auto cond_expr = LowerExpr(std::move(if_stmt->cond));
  auto cond = builder_.CreateCond(cond_expr);
  if (!if_stmt->IsTerminating() && !end_bb && has_else) {
    end_bb = builder_.CreateBB(cond->else_bb);
  }

  builder_.SetInsertBB(cond->then_bb);
  auto then_val = LowerBlock(std::move(if_stmt->block), end_bb);
  if (then_val && lval) {
    builder_.CreateStore(lval, then_val);
  }
  if (!is_then_terminating) {
    if (has_else) {
      builder_.CreateGoto(end_bb);
    } else {
      // jump end_bb
      builder_.CreateGoto(cond->else_bb);
    }
  }

  builder_.SetInsertBB(cond->else_bb);
  if (has_else) {
    bool is_else_terminating = if_stmt->els->IsTerminating();

    std::shared_ptr<mir::RValue> else_val;
    if (if_stmt->IsElseIf()) {
      auto else_if = unique_cast<ast::If>(std::move(if_stmt->els));
      else_val = LowerIf(std::move(else_if), end_bb);
    } else {
      auto else_block = unique_cast<ast::Block>(std::move(if_stmt->els));
      else_val = LowerBlock(std::move(else_block), end_bb);
    }
    if (else_val && lval) {
      builder_.CreateStore(lval, else_val);
    }
    if (!is_else_terminating) {
      builder_.CreateGoto(end_bb);
    }
    if (end_bb) {
      builder_.SetInsertBB(end_bb);
    }
  }
  if (lval) {
    return builder_.CreateLoad(lval);
  }
  return nullptr;
}

std::shared_ptr<mir::RValue> Lower::LowerBlock(
    std::unique_ptr<ast::Block> block, std::shared_ptr<mir::BB> end_bb) {
  std::cout << "LowerBLOCK " << block.get() << std::endl;
  std::shared_ptr<mir::RValue> val = nullptr;
  while (!block->stmts.empty()) {
    auto stmt = block->stmts.move_front();
    auto is_end = block->stmts.empty();
    val = LowerStmt(std::move(stmt), is_end ? end_bb : nullptr);
  }
  std::cout << "END LowerBLOCK " << block.get() << std::endl;
  return val;
}

}  // namespace felis
