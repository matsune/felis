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

#include "error/handler.h"
#include "symtab.h"
#include "syntax/ast.h"

namespace felis {

class Builder {
 public:
  Builder(ErrorHandler &handler, std::string moduleName = "felis")
      : handler_(handler), module_(moduleName, ctx_), builder_(ctx_){};
  bool CreateTargetMachine();
  bool Build(std::unique_ptr<File>);

 private:
  llvm::LLVMContext ctx_;
  llvm::Module module_;
  llvm::IRBuilder<> builder_;
  std::unique_ptr<llvm::TargetMachine> machine_;

  std::shared_ptr<DefFn> currentFn_;

  SymTabManager sm_;
  ErrorHandler &handler_;

  std::shared_ptr<DefFn> InsertDefFn(bool isExt,
                                     const std::unique_ptr<FnProto> &);

  bool Build(std::unique_ptr<FnProto> &, std::shared_ptr<DefFn>);
  bool Build(std::unique_ptr<FnDecl> &, std::shared_ptr<DefFn>);
  bool Build(Block *);
  bool Build(std::unique_ptr<Stmt> &);
  bool Build(Expr *expr, llvm::Value *&value, Ty &ty);

  llvm::Type *getLLVMTyFromTy(Ty ty);
};

}  // namespace felis

#endif  // FELIS_IR_BUILDER_H_
