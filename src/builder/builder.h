#ifndef FELIS_BUILDER_BUILDER_H_
#define FELIS_BUILDER_BUILDER_H_

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Target/TargetMachine.h>

#include <map>
#include <memory>
#include <string>

#include "error/error.h"
#include "node/hir.h"

namespace felis {

class Builder {
 public:
  Builder(std::string module_name, std::string file_name,
          std::unique_ptr<llvm::TargetMachine> machine)
      : module_(module_name, ctx_),
        builder_(ctx_),
        machine_(std::move(machine)) {
    module_.setSourceFileName(file_name);
    module_.setDataLayout(machine_->createDataLayout());
    module_.setTargetTriple(machine_->getTargetTriple().str());
  };

  void Build(std::unique_ptr<hir::File>);

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
  std::map<Decl *, llvm::Value *> decl_map_;

  llvm::Type *LLVMType(const std::shared_ptr<Type> &);

  void RecordValue(std::shared_ptr<Decl> &t, llvm::Value *value) {
    decl_map_[t.get()] = value;
  }

  llvm::Value *GetValue(std::shared_ptr<Decl> &t) {
    return decl_map_.at(t.get());
  }

  llvm::Function *BuildFnProto(std::shared_ptr<Decl> &);

  llvm::Value *BuildStmt(std::unique_ptr<hir::Stmt>);
  void BuildRetStmt(std::unique_ptr<hir::RetStmt>);
  void BuildVarDeclStmt(std::unique_ptr<hir::VarDeclStmt>);
  void BuildAssignStmt(std::unique_ptr<hir::AssignStmt>);

  void BuildBlock(std::unique_ptr<hir::Block>, llvm::AllocaInst *into,
                  llvm::BasicBlock *after_bb);
  void BuildIf(std::unique_ptr<hir::If>, llvm::AllocaInst *into,
               llvm::BasicBlock *after_bb);

  llvm::Value *BuildExpr(std::unique_ptr<hir::Expr>);
  llvm::Constant *BuildConstant(std::unique_ptr<hir::Constant>);
  llvm::Value *BuildBinary(std::unique_ptr<hir::Binary>);

  void EmitCodeGen(std::string, llvm::TargetMachine::CodeGenFileType);
};

}  // namespace felis

#endif  // FELIS_BUILDER_BUILDER_H_
