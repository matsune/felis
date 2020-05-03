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

#include "check/hir.h"
#include "error/error.h"

namespace felis {

class Builder {
 public:
  Builder() : module_("felis", ctx_), builder_(ctx_){};
  bool CreateTargetMachine(std::string &err);
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

  std::map<std::shared_ptr<Decl>, llvm::Value *> declMap_;
  llvm::Type *GetLLVMTyFromTy(std::shared_ptr<Type>);

  llvm::Function *BuildFnProto(std::shared_ptr<Decl>);

  void BuildStmt(std::unique_ptr<hir::Stmt> &);
  void BuildRetStmt(hir::RetStmt *);
  void BuildVarDeclStmt(hir::VarDeclStmt *);
  void BuildAssignStmt(hir::AssignStmt *);
  void BuildIfStmt(hir::IfStmt *);
  void BuildBlock(hir::Block *);

  llvm::Value *BuildExpr(hir::Expr *);
  llvm::Constant *BuildConstant(hir::Constant *);
  llvm::Value *BuildBinary(hir::Binary *);

  void EmitCodeGen(std::string, llvm::TargetMachine::CodeGenFileType);
};

}  // namespace felis

#endif  // FELIS_BUILDER_BUILDER_H_
