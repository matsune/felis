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

#include "check/type_maps.h"
#include "error/error.h"
#include "node/ast.h"

namespace felis {

class LLVMBuilder {
 public:
  LLVMBuilder(std::string module_name, std::string file_name,
              std::unique_ptr<llvm::TargetMachine> machine, TypeMaps &type_maps)
      : module_(module_name, ctx_),
        builder_(ctx_),
        machine_(std::move(machine)),
        type_maps_(type_maps) {
    module_.setSourceFileName(file_name);
    module_.setDataLayout(machine_->createDataLayout());
    module_.setTargetTriple(machine_->getTargetTriple().str());
  };

  void Build(std::unique_ptr<ast::File>);

  void EmitLLVMIR(std::string filename);
  void EmitLLVMBC(std::string filename);
  void EmitASM(std::string filename);
  void EmitOBJ(std::string filename);

 private:
  llvm::LLVMContext ctx_;
  llvm::Module module_;
  llvm::IRBuilder<> builder_;
  std::unique_ptr<llvm::TargetMachine> machine_;

  TypeMaps &type_maps_;
  llvm::Function *function_;
  std::map<std::shared_ptr<Decl>, llvm::Value *> decl_value_map_;

  llvm::Type *LLVMType(const std::shared_ptr<Type> &);
  llvm::FunctionType *LLVMFuncType(const std::shared_ptr<Type> &);
  llvm::Align GetAlign(llvm::Type *);
  llvm::AllocaInst *Alloca(llvm::Type *);
  llvm::AllocaInst *CreateAlloca(llvm::Type *);
  llvm::BasicBlock *CreateBB();

  llvm::Function *CreateFunc(ast::FnProto *);
  llvm::Value *BuildBlock(ast::Block *);
  llvm::Value *BuildStmt(ast::AstNode *);
  void BuildRet(ast::RetStmt *);
  void BuildRetValue(llvm::Value *);
  void BuildVarDecl(ast::VarDeclStmt *);
  void BuildAssign(ast::AssignStmt *);
  llvm::Value *BuildExpr(ast::AstNode *, bool load = true);
  llvm::Value *BuildLit(ast::Literal *);
  llvm::Value *ParseIntLit(ast::Literal *);
  llvm::Value *ParseFloatLit(ast::Literal *);
  llvm::Value *BuildBinary(ast::Binary *);
  llvm::Value *BuildUnary(ast::Unary *);
  llvm::Value *BuildArray(ast::Array *);
  llvm::Value *BuildIf(ast::If *);

  void EmitCodeGen(std::string, llvm::CodeGenFileType);
};

}  // namespace felis

#endif  // FELIS_BACKEND_LLVM_BUILDER_H_
