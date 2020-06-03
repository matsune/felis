#include "llvm_builder.h"

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>

#include "macro.h"
#include "unique.h"

namespace felis {

llvm::Type* LLVMBuilder::LLVMType(const std::shared_ptr<FixedType>& ty) {
  if (auto func_type = std::dynamic_pointer_cast<FuncType>(ty)) {
    std::vector<llvm::Type*> args;
    for (auto& arg : func_type->args) {
      args.push_back(LLVMType(std::dynamic_pointer_cast<FixedType>(arg)));
    }
    return llvm::FunctionType::get(
        LLVMType(std::dynamic_pointer_cast<FixedType>(func_type->ret)), args,
        false);
  }

  if (auto array_type = std::dynamic_pointer_cast<ArrayType>(ty)) {
    auto elem_ty =
        LLVMType(std::dynamic_pointer_cast<FixedType>(array_type->elem));
    return llvm::ArrayType::get(elem_ty, array_type->size);
  }

  if (auto ptr_type = std::dynamic_pointer_cast<PtrType>(ty)) {
    auto elem_ty =
        LLVMType(std::dynamic_pointer_cast<FixedType>(ptr_type->elem));
    return elem_ty->getPointerTo();
  }

  if (ty->IsBool()) return llvm::Type::getInt1Ty(ctx_);
  if (ty->IsI8()) return llvm::Type::getInt8Ty(ctx_);
  if (ty->IsI16()) return llvm::Type::getInt16Ty(ctx_);
  if (ty->IsI32()) return llvm::Type::getInt32Ty(ctx_);
  if (ty->IsI64()) return llvm::Type::getInt64Ty(ctx_);
  if (ty->IsF32()) return llvm::Type::getFloatTy(ctx_);
  if (ty->IsF64()) return llvm::Type::getDoubleTy(ctx_);
  if (ty->IsString()) return llvm::Type::getInt8PtrTy(ctx_);
  if (ty->IsVoid()) return llvm::Type::getVoidTy(ctx_);

  std::cout << ToString(ty) << std::endl;
  UNREACHABLE
}

llvm::Value* LLVMBuilder::GetRValue(std::shared_ptr<mir::RValue> rvalue) {
  std::cout << "GetRValue " << rvalue.get() << std::endl;
  switch (rvalue->RValueKind()) {
    case mir::RValue::Kind::CONST_BOOL: {
      auto v = std::dynamic_pointer_cast<mir::ConstantBool>(rvalue);
      return v->val ? llvm::ConstantInt::getTrue(ctx_)
                    : llvm::ConstantInt::getFalse(ctx_);
    } break;
    case mir::RValue::Kind::CONST_INT: {
      auto v = std::dynamic_pointer_cast<mir::ConstantInt>(rvalue);
      return llvm::ConstantInt::getSigned(LLVMType(v->type), v->val);
    } break;
    case mir::RValue::Kind::CONST_FLOAT: {
      auto v = std::dynamic_pointer_cast<mir::ConstantFloat>(rvalue);
      return llvm::ConstantFP::get(LLVMType(v->type), v->val);
    } break;
    case mir::RValue::Kind::CONST_STRING: {
      auto v = std::dynamic_pointer_cast<mir::ConstantString>(rvalue);
      return builder_.CreateGlobalStringPtr(v->val);
    } break;
    case mir::RValue::Kind::VAL:
      return rvalue_map_.at(rvalue);
  }
}

llvm::Value* LLVMBuilder::GetLValue(std::shared_ptr<mir::LValue> lvalue) {
  std::cout << "GetLValue " << lvalue.get() << std::endl;
  return lvalue_map_.at(lvalue);
}

llvm::BasicBlock* LLVMBuilder::GetBasicBlock(std::shared_ptr<mir::BB> bb) {
  if (auto basic_block = bb_map_[bb]) {
    return basic_block;
  }
  auto basic_block = llvm::BasicBlock::Create(
      ctx_, "bb" + std::to_string(bb->id), current_func_);
  bb_map_[bb] = basic_block;
  return basic_block;
}

llvm::AllocaInst* LLVMBuilder::Alloca(std::shared_ptr<mir::LValue> lval) {
  auto alloca = builder_.CreateAlloca(
      LLVMType(std::dynamic_pointer_cast<FixedType>(lval->type->elem)));
  SetLValue(lval, alloca);
  return alloca;
}

void LLVMBuilder::Load(std::shared_ptr<mir::RValue> rval,
                       std::shared_ptr<mir::LValue> lval) {
  auto ptr = GetLValue(lval);
  auto load = builder_.CreateLoad(ptr);
  SetRValue(rval, load);
}

void LLVMBuilder::Store(std::shared_ptr<mir::RValue> rval,
                        std::shared_ptr<mir::LValue> lval) {
  auto val = GetRValue(rval);
  auto ptr = GetLValue(lval);
  builder_.CreateStore(val, ptr);
}

void LLVMBuilder::Build(std::unique_ptr<mir::File> file) {
  std::cout << "[Build]" << std::endl;
  for (auto func : file->funcs) {
    auto ty = (llvm::FunctionType*)LLVMType(func->type);
    auto fn_value = llvm::Function::Create(
        ty, llvm::GlobalValue::ExternalLinkage, func->name, module_);
    func_map_[func] = fn_value;
  }

  for (auto func : file->funcs) {
    if (func->IsExt()) continue;
    auto function = std::dynamic_pointer_cast<mir::Function>(func);
    std::cout << "function " << function->name << std::endl;
    current_func_ = func_map_.at(func);

    ClearLocalMaps();

    auto bb = function->entry_bb;
    builder_.SetInsertPoint(GetBasicBlock(bb));

    auto arg_it = current_func_->arg_begin();
    for (auto arg : function->args) {
      SetRValue(arg, arg_it);
      ++arg_it;
    }

    while (bb) {
      BuildBB(bb);
      bb = bb->next_bb;
    }

    /* std::string str; */
    /* llvm::raw_string_ostream s(str); */
    auto& out = llvm::outs();
    if (llvm::verifyFunction(*current_func_, &out)) {
      return;
      /* throw CompileError(s.str()); */
    }
  }

  std::cout << "[End Build]" << std::endl;
}

void LLVMBuilder::BuildBB(std::shared_ptr<mir::BB> bb) {
  std::cout << "Build BB" << bb->id << std::endl;
  auto basic_block = GetBasicBlock(bb);
  builder_.SetInsertPoint(basic_block);
  for (auto inst : bb->instructions) {
    BuildInst(inst);
  }
}

void LLVMBuilder::BuildInst(std::shared_ptr<mir::Inst> inst) {
  std::cout << "BuildInst " << inst->InstKind() << std::endl;
  switch (inst->InstKind()) {
    case mir::Inst::ALLOC: {
      auto alloc_inst = std::dynamic_pointer_cast<mir::AllocInst>(inst);
      Alloca(alloc_inst->lval);
    } break;
    case mir::Inst::LOAD: {
      auto load_inst = std::dynamic_pointer_cast<mir::LoadInst>(inst);
      Load(load_inst->rval, load_inst->lval);
    } break;
    case mir::Inst::STORE: {
      auto store_inst = std::dynamic_pointer_cast<mir::StoreInst>(inst);
      Store(store_inst->rval, store_inst->lval);
    } break;
    case mir::Inst::UNARY: {
      auto unary_inst = std::dynamic_pointer_cast<mir::UnaryInst>(inst);
      Unary(unary_inst);
    } break;
    case mir::Inst::BINARY: {
      auto binary_inst = std::dynamic_pointer_cast<mir::BinaryInst>(inst);
      Binary(binary_inst);
    } break;
    case mir::Inst::CMP: {
      auto cmp_inst = std::dynamic_pointer_cast<mir::CmpInst>(inst);
      Cmp(cmp_inst);
    } break;
    case mir::Inst::ARRAY: {
      UNIMPLEMENTED
    } break;
    case mir::Inst::CALL: {
      auto call_inst = std::dynamic_pointer_cast<mir::CallInst>(inst);
      auto func = func_map_.at(call_inst->func);
      std::vector<llvm::Value*> arg_values(call_inst->args.size());
      auto i = 0;
      for (auto arg : call_inst->args) {
        auto val = GetRValue(arg);
        arg_values[i++] = val;
      }
      auto val = builder_.CreateCall(func, arg_values);
      SetRValue(call_inst->val, val);
    } break;
    case mir::Inst::BR: {
      auto br_inst = std::dynamic_pointer_cast<mir::BrInst>(inst);
      auto cond = GetRValue(br_inst->cond);
      auto then_bb = GetBasicBlock(br_inst->then_bb);
      auto else_bb = GetBasicBlock(br_inst->else_bb);
      builder_.CreateCondBr(cond, then_bb, else_bb);
    } break;
    case mir::Inst::GOTO: {
      auto goto_inst = std::dynamic_pointer_cast<mir::GotoInst>(inst);
      auto goto_bb = GetBasicBlock(goto_inst->goto_bb);
      builder_.CreateBr(goto_bb);
    } break;
    case mir::Inst::RET: {
      auto ret_inst = std::dynamic_pointer_cast<mir::RetInst>(inst);
      if (ret_inst->val)
        builder_.CreateRet(GetRValue(ret_inst->val));
      else
        builder_.CreateRetVoid();
    } break;
  }
}

void LLVMBuilder::Unary(std::shared_ptr<mir::UnaryInst> inst) {
  bool is_float = inst->val->type->IsFixedFloat();
  auto expr = GetRValue(inst->operand);
  llvm::Value* val;
  switch (inst->op) {
    case mir::UnaryInst::Op::NEG:
      val = is_float
                ? builder_.CreateFMul(
                      llvm::ConstantFP::get(expr->getType(), -1), expr)

                : builder_.CreateMul(
                      llvm::ConstantInt::getSigned(expr->getType(), -1), expr);
      break;
    case mir::UnaryInst::Op::NOT:
      val = builder_.CreateXor(expr, llvm::ConstantInt::getTrue(ctx_));
      break;
  }
  SetRValue(inst->val, val);
}

void LLVMBuilder::Binary(std::shared_ptr<mir::BinaryInst> inst) {
  auto lhs = GetRValue(inst->lhs);
  auto rhs = GetRValue(inst->rhs);
  auto is_float = inst->lhs->type->IsFixedFloat();

  llvm::Value* val;
  switch (inst->op) {
    case mir::BinaryInst::Op::ADD:
      val = is_float ? builder_.CreateFAdd(lhs, rhs)
                     : builder_.CreateAdd(lhs, rhs);
      break;
    case mir::BinaryInst::Op::SUB:
      val = is_float ? builder_.CreateFSub(lhs, rhs)
                     : builder_.CreateSub(lhs, rhs);
      break;
    case mir::BinaryInst::Op::MUL:
      val = is_float ? builder_.CreateFMul(lhs, rhs)
                     : builder_.CreateMul(lhs, rhs);
      break;
    case mir::BinaryInst::Op::DIV:
      val = is_float ? builder_.CreateFDiv(lhs, rhs)
                     : builder_.CreateSDiv(lhs, rhs);
      break;
    case mir::BinaryInst::Op::MOD:
      val = builder_.CreateSRem(lhs, rhs);
      break;
  }
  SetRValue(inst->val, val);
}

void LLVMBuilder::Cmp(std::shared_ptr<mir::CmpInst> inst) {
  auto lhs = GetRValue(inst->lhs);
  auto rhs = GetRValue(inst->rhs);
  auto is_float = inst->lhs->type->IsFixedFloat();

  llvm::Value* val;
  switch (inst->op) {
    case mir::CmpInst::Op::EQEQ:
      val = is_float ? builder_.CreateFCmpOEQ(lhs, rhs)
                     : builder_.CreateICmpEQ(lhs, rhs);
      break;
    case mir::CmpInst::Op::NEQ:
      val = is_float ? builder_.CreateFCmpONE(lhs, rhs)
                     : builder_.CreateICmpNE(lhs, rhs);
      break;
    case mir::CmpInst::Op::LT:
      val = is_float ? builder_.CreateFCmpOLT(lhs, rhs)
                     : builder_.CreateICmpSLT(lhs, rhs);
      break;
    case mir::CmpInst::Op::LE:
      val = is_float ? builder_.CreateFCmpOLE(lhs, rhs)
                     : builder_.CreateICmpSLE(lhs, rhs);
      break;
    case mir::CmpInst::Op::GT:
      val = is_float ? builder_.CreateFCmpOGT(lhs, rhs)
                     : builder_.CreateICmpSGT(lhs, rhs);
      break;
    case mir::CmpInst::Op::GE:
      val = is_float ? builder_.CreateFCmpOGE(lhs, rhs)
                     : builder_.CreateICmpSGE(lhs, rhs);
      break;
  }
  SetRValue(inst->val, val);
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

void LLVMBuilder::EmitCodeGen(std::string filename,
                              llvm::TargetMachine::CodeGenFileType ft) {
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
  EmitCodeGen(filename,
              llvm::TargetMachine::CodeGenFileType::CGFT_AssemblyFile);
}

void LLVMBuilder::EmitOBJ(std::string filename) {
  EmitCodeGen(filename, llvm::TargetMachine::CodeGenFileType::CGFT_ObjectFile);
}

}  // namespace felis
