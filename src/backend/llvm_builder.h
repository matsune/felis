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

  std::shared_ptr<mir::Function> current_func_;
  std::map<std::shared_ptr<mir::Func>, llvm::Function *> func_map_;
  std::map<std::shared_ptr<mir::BB>, llvm::BasicBlock *> bb_map_;
  std::map<std::shared_ptr<mir::Value>, llvm::Value *> value_map_;

  void SetValue(std::shared_ptr<mir::Value> val, llvm::Value *value) {
    value_map_[val] = value;
  }

  void ClearLocalMaps() {
    bb_map_.clear();
    value_map_.clear();
  }

  void SetCurrentFunction(std::shared_ptr<mir::Function> function) {
    current_func_ = function;

    ClearLocalMaps();

    auto bb = function->entry_bb;
    builder_.SetInsertPoint(GetOrCreateBasicBlock(bb));
  }

  llvm::Function *GetLLVMFunc(std::shared_ptr<mir::Func> func = nullptr) {
    if (!func) func = current_func_;
    return func_map_.at(func);
  }

  llvm::Type *LLVMType(const std::shared_ptr<Type> &);
  llvm::Value *GetValue(std::shared_ptr<mir::Value>);
  llvm::BasicBlock *GetOrCreateBasicBlock(std::shared_ptr<mir::BB>);
  void BuildBB(std::shared_ptr<mir::BB>);
  void BuildInst(std::shared_ptr<mir::Inst>);
  void Load(std::shared_ptr<mir::LoadInst>);
  void Assign(std::shared_ptr<mir::AssignInst>);
  void Unary(std::shared_ptr<mir::UnaryInst>);
  void Binary(std::shared_ptr<mir::BinaryInst>);
  void Cmp(std::shared_ptr<mir::CmpInst>);
  void Array(std::shared_ptr<mir::ArrayInst>);
  void Phi(std::shared_ptr<mir::PhiInst>);
  void Call(std::shared_ptr<mir::CallInst>);
  void Br(std::shared_ptr<mir::BrInst>);
  void Goto(std::shared_ptr<mir::GotoInst>);
  void Ret(std::shared_ptr<mir::RetInst>);
  void EmitCodeGen(std::string, llvm::CodeGenFileType);
};

}  // namespace felis

#endif  // FELIS_BACKEND_LLVM_BUILDER_H_
