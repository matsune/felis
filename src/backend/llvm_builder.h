#ifndef FELIS_BACKEND_LLVM_BUILDER_H_
#define FELIS_BACKEND_LLVM_BUILDER_H_

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Target/TargetMachine.h>

#include <map>
#include <memory>
#include <string>

#include "error/error.h"
#include "node/mir.h"

namespace felis {

class LLVMBuilder {
 public:
  LLVMBuilder(std::string module_name, std::string file_name,
              std::unique_ptr<llvm::TargetMachine> machine)
      : module_(module_name, ctx_),
        builder_(ctx_),
        machine_(std::move(machine)) {
    module_.setSourceFileName(file_name);
    module_.setDataLayout(machine_->createDataLayout());
    module_.setTargetTriple(machine_->getTargetTriple().str());
  };

  void Build(std::unique_ptr<mir::File>);
  void EmitLLVMIR(std::string filename);
  void EmitLLVMBC(std::string filename);
  void EmitASM(std::string filename);
  void EmitOBJ(std::string filename);

 private:
  llvm::LLVMContext ctx_;
  llvm::Module module_;
  llvm::IRBuilder<> builder_;
  std::unique_ptr<llvm::TargetMachine> machine_;
  llvm::Function *current_func_;
  std::map<std::shared_ptr<mir::Func>, llvm::Function *> func_map_;
  std::map<std::shared_ptr<mir::BB>, llvm::BasicBlock *> bb_map_;
  std::map<std::shared_ptr<mir::LValue>, llvm::Value *> lvalue_map_;
  std::map<std::shared_ptr<mir::RValue>, llvm::Value *> rvalue_map_;

  void SetRValue(std::shared_ptr<mir::RValue> rvalue, llvm::Value *value) {
    std::cout << "SetRValue " << rvalue.get() << " value: " << value
              << std::endl;
    rvalue_map_[rvalue] = value;
  }

  void SetLValue(std::shared_ptr<mir::LValue> lvalue, llvm::Value *value) {
    std::cout << "SetLValue " << lvalue.get() << " value: " << value
              << std::endl;
    lvalue_map_[lvalue] = value;
  }

  void ClearLocalMaps() {
    bb_map_.clear();
    lvalue_map_.clear();
    rvalue_map_.clear();
  }

  llvm::Type *LLVMType(const std::shared_ptr<Ty> &);
  llvm::Value *GetRValue(std::shared_ptr<mir::RValue>);
  llvm::Value *GetLValue(std::shared_ptr<mir::LValue>);
  llvm::BasicBlock *GetBasicBlock(std::shared_ptr<mir::BB>);
  void BuildBB(std::shared_ptr<mir::BB>);
  void BuildInst(std::shared_ptr<mir::Inst>);
  llvm::AllocaInst *Alloca(std::shared_ptr<mir::LValue>);
  void Load(std::shared_ptr<mir::RValue>, std::shared_ptr<mir::LValue>);
  void Store(std::shared_ptr<mir::RValue>, std::shared_ptr<mir::LValue>);
  void Unary(std::shared_ptr<mir::UnaryInst>);
  void Binary(std::shared_ptr<mir::BinaryInst>);
  void Cmp(std::shared_ptr<mir::CmpInst>);
  void EmitCodeGen(std::string, llvm::TargetMachine::CodeGenFileType);
};

}  // namespace felis

#endif  // FELIS_BACKEND_LLVM_BUILDER_H_
