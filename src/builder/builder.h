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
  Builder(std::string moduleName, std::string fileName,
          std::unique_ptr<llvm::TargetMachine> machine)
      : module_(moduleName, ctx_),
        builder_(ctx_),
        machine_(std::move(machine)) {
    module_.setSourceFileName(fileName);
    module_.setDataLayout(machine_->createDataLayout());
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
  llvm::Function *currentFunc_;

  std::map<std::shared_ptr<Decl>, llvm::Value *> declMap_;
  llvm::Type *GetLLVMTyFromTy(std::shared_ptr<Type>);

  llvm::Function *BuildFnProto(std::shared_ptr<Decl>);

  void BuildStmt(std::unique_ptr<hir::Stmt>, llvm::BasicBlock *);
  void BuildRetStmt(std::unique_ptr<hir::RetStmt>);
  void BuildVarDeclStmt(std::unique_ptr<hir::VarDeclStmt>);
  void BuildAssignStmt(std::unique_ptr<hir::AssignStmt>);
  void BuildIfStmt(std::unique_ptr<hir::IfStmt>, llvm::BasicBlock *);
  void BuildBlock(std::unique_ptr<hir::Block>, llvm::BasicBlock *);

  llvm::Value *BuildExpr(std::unique_ptr<hir::Expr>);
  llvm::Constant *BuildConstant(std::unique_ptr<hir::Constant>);
  llvm::Value *BuildBinary(std::unique_ptr<hir::Binary>);

  void EmitCodeGen(std::string, llvm::TargetMachine::CodeGenFileType);
};

}  // namespace felis

#endif  // FELIS_BUILDER_BUILDER_H_
