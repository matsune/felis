#ifndef FELIS_IR_BUILDER_H_
#define FELIS_IR_BUILDER_H_

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Target/TargetMachine.h>

#include <map>
#include <memory>
#include <string>

#include "error/error.h"
#include "symtab.h"
#include "syntax/ast.h"

namespace felis {

class Builder {
 public:
  Builder() : module_("felis", ctx_), builder_(ctx_){};
  bool CreateTargetMachine(std::string &err);
  void Build(std::unique_ptr<File>);
  void EmitLLVMIR(std::string filename);
  void EmitLLVMBC(std::string filename);
  void EmitASM(std::string filename);
  void EmitOBJ(std::string filename);

 private:
  llvm::LLVMContext ctx_;
  llvm::Module module_;
  llvm::IRBuilder<> builder_;
  std::unique_ptr<llvm::TargetMachine> machine_;
  SymTabManager sm_;
  std::shared_ptr<DefFn> currentFn_;

  llvm::Type *getLLVMTyFromTy(Ty ty);

  std::shared_ptr<DefFn> InsertDefFn(bool isExt,
                                     const std::unique_ptr<FnProto> &);

  void Build(std::unique_ptr<FnProto> &, std::shared_ptr<DefFn>);
  void Build(std::unique_ptr<FnDecl> &, std::shared_ptr<DefFn>);
  void Build(Block *);
  void Build(std::unique_ptr<Stmt> &);
  void Build(Expr *expr, llvm::Value *&value, Ty &ty);
  void EmitCodeGen(std::string, llvm::TargetMachine::CodeGenFileType);
};

}  // namespace felis

#endif  // FELIS_IR_BUILDER_H_
