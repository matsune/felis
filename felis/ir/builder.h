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

enum EmitType {
  LINK = (1u << 0),
  LLVM_IR = (1u << 1),
  LLVM_BC = (1u << 2),
  ASM = (1u << 3),
  OBJ = (1u << 4),
};

using Emits = uint8_t;

class Builder {
 public:
  Builder(Emits emits = EmitType::LINK)
      : module_("felis", ctx_), builder_(ctx_), emits_(emits){};
  bool CreateTargetMachine(std::string &err);
  void Build(std::unique_ptr<File>);

 private:
  llvm::LLVMContext ctx_;
  llvm::Module module_;
  llvm::IRBuilder<> builder_;
  std::unique_ptr<llvm::TargetMachine> machine_;
  SymTabManager sm_;
  std::shared_ptr<DefFn> currentFn_;
  Emits emits_;

  llvm::Type *getLLVMTyFromTy(Ty ty);

  std::shared_ptr<DefFn> InsertDefFn(bool isExt,
                                     const std::unique_ptr<FnProto> &);

  void Build(std::unique_ptr<FnProto> &, std::shared_ptr<DefFn>);
  void Build(std::unique_ptr<FnDecl> &, std::shared_ptr<DefFn>);
  void Build(Block *);
  void Build(std::unique_ptr<Stmt> &);
  void Build(Expr *expr, llvm::Value *&value, Ty &ty);
};

}  // namespace felis

#endif  // FELIS_IR_BUILDER_H_
