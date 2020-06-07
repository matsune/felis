#include "llvm_builder.h"

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>

#include "macro.h"
#include "unique.h"

namespace felis {

llvm::Type* LLVMBuilder::LLVMType(const std::shared_ptr<Ty>& ty) {
  if (auto func_type = std::dynamic_pointer_cast<FuncTy>(ty)) {
    std::vector<llvm::Type*> args;
    for (auto& arg : func_type->args) {
      args.push_back(LLVMType(arg));
    }
    return llvm::FunctionType::get(LLVMType(func_type->ret), args, false);
  }

  if (auto array_type = std::dynamic_pointer_cast<ArrayTy>(ty)) {
    auto elem_ty = LLVMType(array_type->elem);
    return llvm::ArrayType::get(elem_ty, array_type->size);
  }

  if (auto ptr_type = std::dynamic_pointer_cast<PtrTy>(ty)) {
    auto elem_ty = LLVMType(ptr_type->ref);
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

llvm::Value* LLVMBuilder::GetValue(std::shared_ptr<mir::Value> value,
                                   bool load) {
  std::cout << "GetValue " << value.get() << std::endl;
  switch (value->ValueKind()) {
    case mir::Value::Kind::CONST_BOOL: {
      auto v = std::dynamic_pointer_cast<mir::ConstantBool>(value);
      return v->val ? llvm::ConstantInt::getTrue(ctx_)
                    : llvm::ConstantInt::getFalse(ctx_);
    } break;
    case mir::Value::Kind::CONST_INT: {
      auto v = std::dynamic_pointer_cast<mir::ConstantInt>(value);
      return llvm::ConstantInt::getSigned(LLVMType(v->type), v->val);
    } break;
    case mir::Value::Kind::CONST_FLOAT: {
      auto v = std::dynamic_pointer_cast<mir::ConstantFloat>(value);
      return llvm::ConstantFP::get(LLVMType(v->type), v->val);
    } break;
    case mir::Value::Kind::CONST_STRING: {
      auto v = std::dynamic_pointer_cast<mir::ConstantString>(value);
      return builder_.CreateGlobalStringPtr(v->val);
    } break;
    case mir::Value::Kind::VAR:
      auto llvm_value = value_map_.at(value);
      if (load) {
        auto var = std::dynamic_pointer_cast<mir::Var>(value);
        if (var->alloc)
          return builder_.CreateLoad(LLVMType(var->type), llvm_value);
      }
      return llvm_value;
  }
}

llvm::BasicBlock* LLVMBuilder::GetOrCreateBasicBlock(
    std::shared_ptr<mir::BB> bb) {
  if (auto basic_block = bb_map_[bb]) {
    return basic_block;
  }
  auto basic_block = llvm::BasicBlock::Create(
      ctx_, "bb" + std::to_string(bb->id), GetLLVMFunc());
  bb_map_[bb] = basic_block;
  return basic_block;
}

void LLVMBuilder::Build(std::unique_ptr<mir::File> file) {
  std::cout << "[Build]" << std::endl;
  for (auto func : file->funcs) {
    auto ty = llvm::cast<llvm::FunctionType>(LLVMType(func->type));
    auto fn_value = llvm::Function::Create(
        ty, llvm::GlobalValue::ExternalLinkage, func->name, module_);
    func_map_[func] = fn_value;
  }

  for (auto func : file->funcs) {
    if (func->IsExt()) continue;

    SetCurrentFunction(std::dynamic_pointer_cast<mir::Function>(func));

    std::cout << "VarMap " << func->name << std::endl;
    for (auto it : current_func_->var_list) {
      if (it->alloc) {
        auto alloca = builder_.CreateAlloca(LLVMType(it->type));
        SetValue(it, alloca);
      }
    }

    auto arg_it = GetLLVMFunc()->arg_begin();
    for (auto arg : current_func_->args) {
      SetValue(arg, arg_it);
      ++arg_it;
    }

    // iterate all branches in function
    auto bb = current_func_->entry_bb;
    while (bb) {
      BuildBB(bb);
      bb = bb->next_bb;
    }

    /* std::string str; */
    /* llvm::raw_string_ostream s(str); */
    auto& out = llvm::outs();
    if (llvm::verifyFunction(*GetLLVMFunc(), &out)) {
      return;
      /* throw CompileError(s.str()); */
    }
  }

  std::cout << "[End Build]" << std::endl;
}

void LLVMBuilder::BuildBB(std::shared_ptr<mir::BB> bb) {
  std::cout << "Build BB" << bb->id << std::endl;
  auto basic_block = GetOrCreateBasicBlock(bb);
  builder_.SetInsertPoint(basic_block);
  for (auto inst : bb->instructions) {
    BuildInst(inst);
  }
}

void LLVMBuilder::BuildInst(std::shared_ptr<mir::Inst> inst) {
  std::cout << "BuildInst " << inst->InstKind() << std::endl;
  switch (inst->InstKind()) {
    case mir::Inst::ASSIGN: {
      Assign(std::dynamic_pointer_cast<mir::AssignInst>(inst));
    } break;
    case mir::Inst::UNARY: {
      Unary(std::dynamic_pointer_cast<mir::UnaryInst>(inst));
    } break;
    case mir::Inst::BINARY: {
      Binary(std::dynamic_pointer_cast<mir::BinaryInst>(inst));
    } break;
    case mir::Inst::CMP: {
      Cmp(std::dynamic_pointer_cast<mir::CmpInst>(inst));
    } break;
    case mir::Inst::GEP: {
      Gep(std::dynamic_pointer_cast<mir::GepInst>(inst));
    } break;
    case mir::Inst::CALL: {
      Call(std::dynamic_pointer_cast<mir::CallInst>(inst));
    } break;
    case mir::Inst::BR: {
      Br(std::dynamic_pointer_cast<mir::BrInst>(inst));
    } break;
    case mir::Inst::GOTO: {
      Goto(std::dynamic_pointer_cast<mir::GotoInst>(inst));
    } break;
    case mir::Inst::RET: {
      Ret(std::dynamic_pointer_cast<mir::RetInst>(inst));
    } break;
  }
}

void LLVMBuilder::Assign(std::shared_ptr<mir::AssignInst> inst) {
  if (auto array_ty = std::dynamic_pointer_cast<ArrayTy>(inst->value->type)) {
    auto val = GetValue(inst->value, false);
    auto into = GetValue(inst->into, false);
    auto val_i8ptr =
        builder_.CreateBitCast(val, llvm::Type::getInt8PtrTy(ctx_));
    auto into_i8ptr =
        builder_.CreateBitCast(into, llvm::Type::getInt8PtrTy(ctx_));
    builder_.CreateMemCpy(
        into_i8ptr, llvm::Align(4), val_i8ptr, llvm::Align(4),
        module_.getDataLayout().getTypeAllocSize(LLVMType(array_ty->elem)));
  } else {
    auto val = GetValue(inst->value, true);
    auto into = GetValue(inst->into, false);
    builder_.CreateStore(val, into);
  }
}

void LLVMBuilder::Unary(std::shared_ptr<mir::UnaryInst> inst) {
  bool is_float = inst->var->type->IsFloat();
  auto expr = GetValue(inst->operand, true);
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
  SetValue(inst->var, val);
}

void LLVMBuilder::Binary(std::shared_ptr<mir::BinaryInst> inst) {
  auto lhs = GetValue(inst->lhs, true);
  auto rhs = GetValue(inst->rhs, true);
  auto is_float = inst->lhs->type->IsFloat();

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
  SetValue(inst->var, val);
}

void LLVMBuilder::Cmp(std::shared_ptr<mir::CmpInst> inst) {
  auto lhs = GetValue(inst->lhs, true);
  auto rhs = GetValue(inst->rhs, true);
  auto is_float = inst->lhs->type->IsFloat();

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
  SetValue(inst->var, val);
}

void LLVMBuilder::Gep(std::shared_ptr<mir::GepInst> inst) {
  assert(inst->arr->alloc);
  if (auto array_type = std::dynamic_pointer_cast<ArrayTy>(inst->arr->type)) {
    auto value = builder_.CreateInBoundsGEP(
        LLVMType(array_type), GetValue(inst->arr, false),
        {llvm::ConstantInt::getSigned(llvm::Type::getInt64Ty(ctx_), 0),
         llvm::ConstantInt::getSigned(llvm::Type::getInt64Ty(ctx_),
                                      inst->idx)});
    SetValue(inst->var, value);
  } else {
    UNIMPLEMENTED
  }
}

void LLVMBuilder::Call(std::shared_ptr<mir::CallInst> inst) {
  auto func = func_map_.at(inst->func);
  std::vector<llvm::Value*> arg_values(inst->args.size());
  auto i = 0;
  for (auto arg : inst->args) {
    auto val = GetValue(arg, true);
    arg_values[i++] = val;
  }
  auto val = builder_.CreateCall(func, arg_values);
  SetValue(inst->var, val);
}

void LLVMBuilder::Br(std::shared_ptr<mir::BrInst> inst) {
  auto cond = GetValue(inst->cond, true);
  auto then_bb = GetOrCreateBasicBlock(inst->then_bb);
  auto else_bb = GetOrCreateBasicBlock(inst->else_bb);
  builder_.CreateCondBr(cond, then_bb, else_bb);
}

void LLVMBuilder::Goto(std::shared_ptr<mir::GotoInst> inst) {
  auto goto_bb = GetOrCreateBasicBlock(inst->goto_bb);
  builder_.CreateBr(goto_bb);
}

void LLVMBuilder::Ret(std::shared_ptr<mir::RetInst> inst) {
  if (inst->val)
    builder_.CreateRet(GetValue(inst->val, true));
  else
    builder_.CreateRetVoid();
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
