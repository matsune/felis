#include "llvm_builder.h"

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>

#include "check/parse.h"
#include "macro.h"
#include "unique.h"

namespace felis {

llvm::Type* LLVMBuilder::LLVMType(const std::shared_ptr<Type>& type) {
  if (type->IsBool()) return llvm::Type::getInt1Ty(ctx_);
  if (type->IsI8()) return llvm::Type::getInt8Ty(ctx_);
  if (type->IsI16()) return llvm::Type::getInt16Ty(ctx_);
  if (type->IsI32()) return llvm::Type::getInt32Ty(ctx_);
  if (type->IsI64()) return llvm::Type::getInt64Ty(ctx_);
  if (type->IsF32()) return llvm::Type::getFloatTy(ctx_);
  if (type->IsF64()) return llvm::Type::getDoubleTy(ctx_);
  if (type->IsString()) return llvm::Type::getInt8PtrTy(ctx_);
  if (type->IsVoid()) return llvm::Type::getVoidTy(ctx_);
  if (type->IsFunc()) {
    std::vector<llvm::Type*> args;
    for (auto& arg : type->GetArgs()) {
      args.push_back(LLVMType(arg));
    }
    return llvm::FunctionType::get(LLVMType(type->GetRet()), args, false);
  }
  if (type->IsArray()) {
    auto elem_ty = LLVMType(type->GetElem());
    return llvm::ArrayType::get(elem_ty, type->GetSize());
  }
  if (type->IsPtr()) {
    auto elem_ty = LLVMType(type->GetElem());
    return elem_ty->getPointerTo();
  }
  UNREACHABLE
}

llvm::Align GetAlign(llvm::Type* ty) {
  if (ty->isArrayTy()) return GetAlign(ty->getArrayElementType());
  if (ty->isPointerTy()) return GetAlign(ty->getPointerElementType());
  return llvm::Align(ty->getPrimitiveSizeInBits() / 8);
}

llvm::Function* LLVMBuilder::CreateFunc(ast::FnProto* proto) {
  auto decl = type_maps_.GetDecl(proto->name);
  auto type = llvm::cast<llvm::FunctionType>(LLVMType(decl->type));
  return llvm::Function::Create(type, llvm::GlobalValue::ExternalLinkage,
                                proto->name->val, module_);
}

void LLVMBuilder::Build(std::unique_ptr<ast::File> file) {
  for (auto ext : file->externs) {
    auto decl = type_maps_.GetDecl(ext->proto->name);
    decl_value_map_[decl] = CreateFunc(ext->proto);
  }
  for (auto func : file->funcs) {
    auto decl = type_maps_.GetDecl(func->proto->name);
    decl_value_map_[decl] = CreateFunc(func->proto);
  }

  for (auto func : file->funcs) {
    auto func_decl = type_maps_.GetDecl(func->proto->name);
    function_ = llvm::cast<llvm::Function>(decl_value_map_.at(func_decl));
    auto bb = llvm::BasicBlock::Create(ctx_, "", function_);
    builder_.SetInsertPoint(bb);

    for (auto i = 0; i < func->proto->args->list.size(); ++i) {
      auto arg = func->proto->args->list.at(i);
      auto decl = type_maps_.GetDecl(arg->name);
      auto alloca =
          new llvm::AllocaInst(function_->getArg(i)->getType(), 0, nullptr,
                               GetAlign(function_->getArg(i)->getType()));
      builder_.Insert(alloca);
      builder_.CreateStore(function_->getArg(i), alloca);
      decl_value_map_[decl] = alloca;
    }

    auto value = BuildBlock(func->block);
    if (!builder_.GetInsertBlock()->getTerminator()) {
      if (func_decl->type->GetRet()->IsVoid()) {
        builder_.CreateRetVoid();
      } else {
        builder_.CreateRet(value);
      }
    }
  }
}

llvm::Value* LLVMBuilder::BuildBlock(ast::Block* block) {
  llvm::Value* value;
  for (auto stmt : block->stmts) {
    value = BuildStmt(stmt);
  }
  return value;
}

llvm::Value* LLVMBuilder::BuildStmt(ast::AstNode* stmt) {
  if (auto ret = node_cast_ornull<ast::RetStmt>(stmt)) {
    BuildRet(ret);
  } else if (auto var_decl = node_cast_ornull<ast::VarDeclStmt>(stmt)) {
    BuildVarDecl(var_decl);
  } else if (auto assign = node_cast_ornull<ast::AssignStmt>(stmt)) {
    BuildAssign(assign);
  } else {
    return BuildExpr(stmt);
  }
  return nullptr;
}

void LLVMBuilder::BuildRet(ast::RetStmt* stmt) {
  if (stmt->expr) {
    builder_.CreateRet(BuildExpr(stmt->expr));
  } else {
    builder_.CreateRetVoid();
  }
}

llvm::Value* LLVMBuilder::GetLValue(ast::AstNode* expr) {
  if (auto ident = node_cast_ornull<ast::Ident>(expr)) {
    auto decl = type_maps_.GetDecl(ident);
    return decl_value_map_.at(decl);
  } else if (auto index = node_cast_ornull<ast::Index>(expr)) {
    auto val = GetLValue(index->expr);
    auto idx = BuildExpr(index->idx_expr);
    return builder_.CreateInBoundsGEP(
        val->getType()->getPointerElementType(), val,
        {llvm::ConstantInt::getSigned(llvm::Type::getInt64Ty(ctx_), 0), idx});
  } else {
    UNREACHABLE
  }
}

void LLVMBuilder::BuildVarDecl(ast::VarDeclStmt* stmt) {
  auto val = BuildExpr(stmt->expr);
  auto alloca = new llvm::AllocaInst(val->getType(), 0, nullptr,
                                     GetAlign(val->getType()));
  builder_.Insert(alloca);
  builder_.CreateStore(val, alloca);
  decl_value_map_[type_maps_.GetDecl(stmt->name)] = alloca;
}

void LLVMBuilder::BuildAssign(ast::AssignStmt* stmt) {
  auto lval = GetLValue(stmt->left);
  auto val = BuildExpr(stmt->expr);
  builder_.CreateStore(val, lval);
}

llvm::Value* LLVMBuilder::BuildExpr(ast::AstNode* expr) {
  if (auto lit = node_cast_ornull<ast::Literal>(expr)) {
    return BuildLit(lit);
  } else if (auto ident = node_cast_ornull<ast::Ident>(expr)) {
    return builder_.CreateLoad(BuildIdent(ident));
  } else if (auto binary = node_cast_ornull<ast::Binary>(expr)) {
    return BuildBinary(binary);
  } else if (auto unary = node_cast_ornull<ast::Unary>(expr)) {
    return BuildUnary(unary);
  } else if (auto call = node_cast_ornull<ast::Call>(expr)) {
    auto decl = type_maps_.GetDecl(call->ident);
    auto func = llvm::cast<llvm::Function>(decl_value_map_.at(decl));
    std::vector<llvm::Value*> arg_values;
    for (auto arg : call->args) {
      arg_values.push_back(BuildExpr(arg));
    }
    return builder_.CreateCall(func, arg_values);
  } else if (auto array = node_cast_ornull<ast::Array>(expr)) {
    return BuildArray(array);
  } else if (auto if_stmt = node_cast_ornull<ast::If>(expr)) {
    return BuildIf(if_stmt);
  } else if (auto block = node_cast_ornull<ast::Block>(expr)) {
    return BuildBlock(block);
  } else if (auto index = node_cast_ornull<ast::Index>(expr)) {
    return BuildIndex(index);
  } else {
    UNREACHABLE
  }
}

llvm::Value* LLVMBuilder::BuildLit(ast::Literal* lit) {
  switch (lit->kind) {
    case ast::Literal::Kind::CHAR: {
      auto ty = LLVMType(type_maps_.GetResult(lit).type);

      if (ty->isIntegerTy()) {
        return llvm::ConstantInt::get(ty, lit->r);
      } else if (ty->isFloatTy()) {
        return llvm::ConstantFP::get(ty, lit->r);
      } else {
        UNREACHABLE
      }
    } break;
    case ast::Literal::Kind::INT: {
      return ParseIntLit(lit);
    } break;
    case ast::Literal::Kind::FLOAT: {
      return ParseFloatLit(lit);
    } break;
    case ast::Literal::Kind::BOOL: {
      return lit->val == "true" ? llvm::ConstantInt::getTrue(ctx_)
                                : llvm::ConstantInt::getFalse(ctx_);
    } break;
    case ast::Literal::Kind::STRING: {
      return builder_.CreateGlobalStringPtr(lit->val);
    } break;
  }
}

llvm::Value* LLVMBuilder::ParseIntLit(ast::Literal* lit) {
  int64_t n;
  std::string err;
  if (!ParseInt(lit->val, n, err)) {
    throw LocError::Create(lit->begin, err);
  }
  auto ty = type_maps_.GetResult(lit).type;
  if (ty->IsI8()) {
    if (n < INT8_MIN || n > INT8_MAX) {
      throw LocError::Create(lit->begin, "overflow int8");
    }
    return llvm::ConstantInt::get(LLVMType(ty), n);
  } else if (ty->IsI16()) {
    if (n < INT16_MIN || n > INT16_MAX) {
      throw LocError::Create(lit->begin, "overflow int16");
    }
    return llvm::ConstantInt::get(LLVMType(ty), n);
  } else if (ty->IsI32()) {
    if (n < INT32_MIN || n > INT32_MAX) {
      throw LocError::Create(lit->begin, "overflow int32");
    }
    return llvm::ConstantInt::get(LLVMType(ty), n);
  } else if (ty->IsI64()) {
    return llvm::ConstantInt::get(LLVMType(ty), n);
  } else if (ty->IsF32() || ty->IsF64()) {
    return llvm::ConstantFP::get(LLVMType(ty), n);
  } else {
    UNREACHABLE
  }
}

llvm::Value* LLVMBuilder::ParseFloatLit(ast::Literal* lit) {
  std::string err;
  double n;
  if (!ParseFloat(lit->val, n, err)) {
    throw LocError::Create(lit->begin, err);
  }
  auto ty = type_maps_.GetResult(lit).type;
  return llvm::ConstantFP::get(LLVMType(ty), n);
}

llvm::Value* LLVMBuilder::BuildIdent(ast::Ident* ident) {
  auto decl = type_maps_.GetDecl(ident);
  return decl_value_map_.at(decl);
}

llvm::Value* LLVMBuilder::BuildBinary(ast::Binary* binary) {
  auto lhs = BuildExpr(binary->lhs);
  auto rhs = BuildExpr(binary->rhs);
  bool is_float = type_maps_.GetResult(binary).type->IsFloat();
  switch (binary->op->kind) {
    case ast::BinaryOp::Kind::EQEQ:
      return is_float ? builder_.CreateFCmpOEQ(lhs, rhs)
                      : builder_.CreateICmpEQ(lhs, rhs);
    case ast::BinaryOp::Kind::NEQ:
      return is_float ? builder_.CreateFCmpONE(lhs, rhs)
                      : builder_.CreateICmpNE(lhs, rhs);
    case ast::BinaryOp::Kind::LT:
      return is_float ? builder_.CreateFCmpOLT(lhs, rhs)
                      : builder_.CreateICmpSLT(lhs, rhs);
    case ast::BinaryOp::Kind::LE:
      return is_float ? builder_.CreateFCmpOLE(lhs, rhs)
                      : builder_.CreateICmpSLE(lhs, rhs);
    case ast::BinaryOp::Kind::GT:
      return is_float ? builder_.CreateFCmpOGT(lhs, rhs)
                      : builder_.CreateICmpSGT(lhs, rhs);
    case ast::BinaryOp::Kind::GE:
      return is_float ? builder_.CreateFCmpOGE(lhs, rhs)
                      : builder_.CreateICmpSGE(lhs, rhs);
    case ast::BinaryOp::Kind::ADD:
      return is_float ? builder_.CreateFAdd(lhs, rhs)
                      : builder_.CreateAdd(lhs, rhs);
      break;
    case ast::BinaryOp::Kind::SUB:
      return is_float ? builder_.CreateFSub(lhs, rhs)
                      : builder_.CreateSub(lhs, rhs);
      break;
    case ast::BinaryOp::Kind::MUL:
      return is_float ? builder_.CreateFMul(lhs, rhs)
                      : builder_.CreateMul(lhs, rhs);
      break;
    case ast::BinaryOp::Kind::DIV:
      return is_float ? builder_.CreateFDiv(lhs, rhs)
                      : builder_.CreateSDiv(lhs, rhs);
      break;
    case ast::BinaryOp::Kind::MOD:
      return builder_.CreateSRem(lhs, rhs);
      break;
  }
}

llvm::Value* LLVMBuilder::BuildUnary(ast::Unary* unary) {
  auto val = BuildExpr(unary->expr);
  auto is_float = type_maps_.GetResult(unary).type->IsFloat();
  switch (unary->op->kind) {
    case ast::UnaryOp::Kind::NEG:
      return is_float
                 ? builder_.CreateFMul(
                       llvm::ConstantFP::get(val->getType(), -1), val)

                 : builder_.CreateMul(
                       llvm::ConstantInt::getSigned(val->getType(), -1), val);
    case ast::UnaryOp::Kind::NOT:
      return builder_.CreateXor(val, llvm::ConstantInt::getTrue(ctx_));
  }
}

llvm::Value* LLVMBuilder::BuildArray(ast::Array* array) {
  auto type = type_maps_.GetResult(array).type;
  auto alloca = builder_.CreateAlloca(LLVMType(type));
  for (unsigned long idx = 0; idx < array->exprs.size(); ++idx) {
    auto gep = builder_.CreateInBoundsGEP(
        alloca->getType()->getPointerElementType(), alloca,
        {llvm::ConstantInt::getSigned(llvm::Type::getInt64Ty(ctx_), 0),
         llvm::ConstantInt::getSigned(llvm::Type::getInt64Ty(ctx_), idx)});
    auto val = BuildExpr(array->exprs.at(idx));
    builder_.CreateStore(val, gep);
  }
  return builder_.CreateLoad(alloca);
}

llvm::Value* LLVMBuilder::BuildIf(ast::If* if_stmt) {
  // store current BB here to insert condBr later
  llvm::BasicBlock* cond_bb = builder_.GetInsertBlock();

  bool has_else = if_stmt->HasElse();
  auto stmt_res = type_maps_.GetResult(if_stmt);

  // build cond and then-block
  auto cond = BuildExpr(if_stmt->cond);
  auto then_bb = llvm::BasicBlock::Create(ctx_, "", function_);
  builder_.SetInsertPoint(then_bb);
  auto block_val = BuildBlock(if_stmt->block);

  if (!has_else) {
    llvm::BasicBlock* end_bb = llvm::BasicBlock::Create(ctx_, "", function_);
    if (!builder_.GetInsertBlock()->getTerminator()) {
      builder_.CreateBr(end_bb);
    }

    // back to then_bb and create cond br
    builder_.SetInsertPoint(cond_bb);
    builder_.CreateCondBr(cond, then_bb, end_bb);

    builder_.SetInsertPoint(end_bb);
    return nullptr;
  }

  if (stmt_res.IsRet()) {
    // terminating
    auto else_bb = llvm::BasicBlock::Create(ctx_, "", function_);
    builder_.SetInsertPoint(else_bb);
    if (if_stmt->IsElseIf()) {
      BuildIf(node_cast<ast::If>(if_stmt->els));
    } else {
      BuildBlock(node_cast<ast::Block>(if_stmt->els));
    }

    // back to then_bb and create cond br
    builder_.SetInsertPoint(cond_bb);
    builder_.CreateCondBr(cond, then_bb, else_bb);

    return nullptr;
  }

  auto block_res = type_maps_.GetResult(if_stmt->block);
  auto block_phi_bb = builder_.GetInsertBlock();

  auto else_bb = llvm::BasicBlock::Create(ctx_, "", function_);
  builder_.SetInsertPoint(else_bb);
  llvm::Value* else_val;
  if (if_stmt->IsElseIf()) {
    else_val = BuildIf(node_cast<ast::If>(if_stmt->els));
  } else {
    else_val = BuildBlock(node_cast<ast::Block>(if_stmt->els));
  }
  auto else_res = type_maps_.GetResult(if_stmt->els);
  auto else_phi_bb = builder_.GetInsertBlock();

  auto end_bb = llvm::BasicBlock::Create(ctx_, "", function_);
  // after creating all blocks, back to each block and create br
  if (!block_phi_bb->getTerminator()) {
    builder_.SetInsertPoint(block_phi_bb);
    builder_.CreateBr(end_bb);
  }
  if (!else_phi_bb->getTerminator()) {
    builder_.SetInsertPoint(else_phi_bb);
    builder_.CreateBr(end_bb);
  }

  // back to then_bb and create cond br
  builder_.SetInsertPoint(cond_bb);
  builder_.CreateCondBr(cond, then_bb, else_bb);

  builder_.SetInsertPoint(end_bb);
  if (stmt_res.IsExpr()) {
    auto nodes = 0;
    if (block_res.IsExpr()) ++nodes;
    if (else_res.IsExpr()) ++nodes;
    auto phi = builder_.CreatePHI(LLVMType(stmt_res.type), nodes);
    if (block_res.IsExpr()) phi->addIncoming(block_val, block_phi_bb);
    if (else_res.IsExpr()) phi->addIncoming(else_val, else_phi_bb);
    return phi;
  }
  return nullptr;
}

llvm::Value* LLVMBuilder::BuildIndex(ast::Index* index) {
  auto lval = GetLValue(index);
  return builder_.CreateLoad(lval);
}

void LLVMBuilder::EmitLLVMIR(std::string filename) {
  std::error_code err_code;
  llvm::raw_fd_ostream out(filename, err_code);
  if (err_code) {
    throw std::runtime_error(err_code.message());
  }
  module_.print(out, nullptr);
}

void LLVMBuilder::EmitLLVMBC(std::string filename) {
  std::error_code err_code;
  llvm::raw_fd_ostream out(filename, err_code);
  if (err_code) {
    throw std::runtime_error(err_code.message());
  }
  llvm::WriteBitcodeToFile(module_, out);
}

void LLVMBuilder::EmitCodeGen(std::string filename, llvm::CodeGenFileType ft) {
  std::error_code err_code;
  llvm::raw_fd_ostream out(filename, err_code);
  if (err_code) {
    throw std::runtime_error(err_code.message());
  }
  llvm::legacy::PassManager pass;
  if (machine_->addPassesToEmitFile(pass, out, nullptr, ft)) {
    throw std::runtime_error("TargetMachine can't emit a file of this type");
  }
  pass.run(module_);
  out.flush();
}

void LLVMBuilder::EmitASM(std::string filename) {
  EmitCodeGen(filename, llvm::CodeGenFileType::CGFT_AssemblyFile);
}

void LLVMBuilder::EmitOBJ(std::string filename) {
  EmitCodeGen(filename, llvm::CodeGenFileType::CGFT_ObjectFile);
}

}  // namespace felis
