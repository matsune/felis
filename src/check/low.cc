
#include "check/low.h"

#include <sstream>

#include "check/parse.h"
#include "error/error.h"

namespace felis {

std::shared_ptr<mir::Func> MIRBuilder::CreateFunc(std::shared_ptr<Decl> decl) {
  bool is_ext = decl->kind == DeclKind::EXT;
  mir::FunctionID id = fn_decls.size();
  auto func =
      is_ext
          ? std::make_shared<mir::Func>(id, decl->name, decl->AsFuncType())
          : std::make_shared<mir::Function>(id, decl->name, decl->AsFuncType());
  file->funcs.push_back(func);
  fn_decls[decl] = func;
  return func;
}

std::shared_ptr<mir::BB> MIRBuilder::CreateBlock() {
  assert(current_bb);
  auto bb = std::make_shared<mir::BB>(current_bb->parent);
  auto next = current_bb->next_bb;
  bb->next_bb = next;
  current_bb->next_bb = bb;
  return bb;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateLoad(std::shared_ptr<Decl> decl) {
  auto val = CreateVal(decl->type);
  auto right = GetAlloc(decl);
  current_bb->instructions.push_back(std::make_shared<mir::Load>(val, right));
  return val;
}

std::shared_ptr<mir::Alloc> MIRBuilder::CreateAlloc(
    std::shared_ptr<Decl> decl) {
  auto id = current_bb->parent.vals.size();
  auto alloc = std::make_shared<mir::Alloc>(id, decl->type);
  current_bb->parent.vals.push_back(alloc);
  alloc_map[decl] = alloc;
  return alloc;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateVal(std::shared_ptr<Type> type) {
  auto id = current_bb->parent.vals.size();
  auto val = std::make_shared<mir::Val>(id, type);
  current_bb->parent.vals.push_back(val);
  return val;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateBinary(
    std::shared_ptr<Type> ty, std::shared_ptr<mir::Value> lhs,
    std::shared_ptr<mir::Value> rhs, mir::Binary::Op op) {
  auto val = CreateVal(ty);
  auto binary = std::make_shared<mir::Binary>(val, lhs, rhs, op);
  current_bb->instructions.push_back(binary);
  return val;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateComp(
    std::shared_ptr<Type> ty, std::shared_ptr<mir::Value> lhs,
    std::shared_ptr<mir::Value> rhs, mir::Comp::Op op) {
  auto val = CreateVal(ty);
  auto comp = std::make_shared<mir::Comp>(val, lhs, rhs, op);
  current_bb->instructions.push_back(comp);
  return val;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateUnary(
    std::shared_ptr<Type> ty, std::shared_ptr<mir::Value> value,
    mir::Unary::Op op) {
  auto val = CreateVal(ty);
  auto comp = std::make_shared<mir::Unary>(val, value, op);
  current_bb->instructions.push_back(comp);
  return val;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateArray(
    std::shared_ptr<Type> type,
    std::vector<std::shared_ptr<mir::Value>> values) {
  auto val = CreateVal(type);
  auto array = std::make_shared<mir::Array>(val, values);
  current_bb->instructions.push_back(array);
  return val;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateCall(
    std::shared_ptr<Decl> decl, std::vector<std::shared_ptr<mir::Value>> args) {
  auto val = CreateVal(decl->AsFuncType()->ret);
  auto func = GetFunction(decl);
  current_bb->instructions.push_back(
      std::make_shared<mir::Call>(val, args, func));
  return val;
}

void MIRBuilder::CreateStore(std::shared_ptr<mir::Alloc> alloc,
                             std::shared_ptr<mir::Value> value) {
  auto assign_inst = std::make_shared<mir::Store>(alloc, value);
  current_bb->instructions.push_back(assign_inst);
}

void MIRBuilder::CreateRet(std::shared_ptr<mir::Value> value) {
  auto ret = std::make_shared<mir::Ret>(value);
  current_bb->instructions.push_back(ret);
}

std::shared_ptr<mir::Cond> MIRBuilder::CreateCond(
    std::shared_ptr<mir::Value> cond) {
  auto then_bb = CreateBlock();
  auto otherwise_bb = CreateBlock();
  return std::make_shared<mir::Cond>(cond, then_bb, otherwise_bb);
}

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

  return Lower(ident_decl_map, expr_type_map).Lowering(std::move(file));
}

std::unique_ptr<mir::File> Lower::Lowering(std::unique_ptr<ast::File> file) {
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
    builder_.SetInsertPoint(func->entry_bb);
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
    builder_.SetInsertPoint(function->entry_bb);

    LowerBlock(std::move(fn_decl->block));
  }
  return std::move(builder_.file);
}

std::shared_ptr<mir::Value> Lower::LowerBlock(
    std::unique_ptr<ast::Block> block) {
  std::shared_ptr<mir::Value> val;
  while (!block->stmts.empty()) {
    auto stmt = block->stmts.move_front();
    val = LowerStmt(std::move(stmt));
  }
  return val;
}

std::shared_ptr<mir::Value> Lower::LowerStmt(std::unique_ptr<ast::Stmt> stmt) {
  std::cout << "LowerStmt " << ToString(stmt->StmtKind()) << std::endl;
  switch (stmt->StmtKind()) {
    case ast::Stmt::Kind::EXPR:
      return LowerExpr(unique_cast<ast::Expr>(std::move(stmt)));
    case ast::Stmt::Kind::ASSIGN: {
      auto assign = unique_cast<ast::AssignStmt>(std::move(stmt));
      auto decl = GetDecl(assign->name);
      auto alloc = builder_.GetAlloc(decl);
      auto value = LowerExpr(std::move(assign->expr));
      builder_.CreateStore(alloc, value);
      return alloc;
    } break;
    case ast::Stmt::Kind::VAR_DECL: {
      auto var_decl = unique_cast<ast::VarDeclStmt>(std::move(stmt));
      auto decl = GetDecl(var_decl->name);
      auto alloc = builder_.CreateAlloc(decl);
      auto value = LowerExpr(std::move(var_decl->expr));
      builder_.CreateStore(alloc, value);
      return alloc;
    } break;
    case ast::Stmt::Kind::RET: {
      auto ret = unique_cast<ast::RetStmt>(std::move(stmt));
      std::shared_ptr<mir::Value> val = nullptr;
      if (ret->expr) {
        val = LowerExpr(std::move(ret->expr));
      }
      builder_.CreateRet(val);
      return nullptr;
    } break;
  }
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

bool IsCompOp(ast::BinaryOp::Op op) {
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

mir::Binary::Op BinOp(ast::BinaryOp::Op op) {
  switch (op) {
    case ast::BinaryOp::Op::ADD:
      return mir::Binary::Op::ADD;
    case ast::BinaryOp::Op::SUB:
      return mir::Binary::Op::SUB;
    case ast::BinaryOp::Op::MUL:
      return mir::Binary::Op::MUL;
    case ast::BinaryOp::Op::DIV:
      return mir::Binary::Op::DIV;
    case ast::BinaryOp::Op::MOD:
      return mir::Binary::Op::MOD;
    default:
      UNREACHABLE
  }
}

mir::Comp::Op CompOp(ast::BinaryOp::Op op) {
  switch (op) {
    case ast::BinaryOp::Op::EQEQ:
      return mir::Comp::Op::EQEQ;
    case ast::BinaryOp::Op::NEQ:
      return mir::Comp::Op::NEQ;
    case ast::BinaryOp::Op::LT:
      return mir::Comp::Op::LT;
    case ast::BinaryOp::Op::LE:
      return mir::Comp::Op::LE;
    case ast::BinaryOp::Op::GT:
      return mir::Comp::Op::GT;
    case ast::BinaryOp::Op::GE:
      return mir::Comp::Op::GE;
    default:
      UNREACHABLE
  }
}

mir::Unary::Op UnaryOp(ast::UnaryOp::Op op) {
  switch (op) {
    case ast::UnaryOp::Op::NEG:
      return mir::Unary::Op::NEG;
    case ast::UnaryOp::Op::NOT:
      return mir::Unary::Op::NOT;
  }
}

std::shared_ptr<mir::Value> Lower::LowerExpr(std::unique_ptr<ast::Expr> expr) {
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
        return builder_.CreateBinary(ty, lhs, rhs, op);
      } else if (IsCompOp(binary->op->op)) {
        auto op = CompOp(binary->op->op);
        return builder_.CreateComp(ty, lhs, rhs, op);
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
      std::vector<std::shared_ptr<mir::Value>> args;
      while (!call->args.empty()) {
        args.push_back(LowerExpr(call->args.move_front()));
      }
      return builder_.CreateCall(decl, std::move(args));
    } break;
    case ast::Expr::Kind::UNARY: {
      auto unary = unique_cast<ast::UnaryExpr>(std::move(expr));
      auto ty = GetType(unary);
      auto expr = LowerExpr(std::move(unary->expr));
      return builder_.CreateUnary(ty, expr, UnaryOp(unary->op->op));
    } break;
    case ast::Expr::Kind::BLOCK: {
      return LowerBlock(unique_cast<ast::Block>(std::move(expr)));
    } break;
    case ast::Expr::Kind::IF: {
      return LowerIf(unique_cast<ast::If>(std::move(expr)));
    } break;
    case ast::Expr::Kind::ARRAY: {
      auto array = unique_cast<ast::ArrayExpr>(std::move(expr));
      auto type = GetType(array);
      std::vector<std::shared_ptr<mir::Value>> values;
      while (!array->exprs.empty()) {
        auto expr = array->exprs.move_front();
        values.push_back(LowerExpr(std::move(expr)));
      }
      return builder_.CreateArray(type, values);

    } break;
  }
  UNREACHABLE
}

std::unique_ptr<mir::Value> Lower::LowerLit(std::unique_ptr<ast::Lit> lit) {
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

std::shared_ptr<mir::Value> Lower::LowerIf(std::unique_ptr<ast::If> if_stmt) {
  auto cond_expr = LowerExpr(std::move(if_stmt->cond));
  auto cond = builder_.CreateCond(cond_expr);
  UNIMPLEMENTED
}

}  // namespace felis
