#ifndef FELIS_INTERNAL_FELIS_LIB_H_
#define FELIS_INTERNAL_FELIS_LIB_H_

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include <map>

namespace felis {

class FelisLib {
 public:
  FelisLib(llvm::LLVMContext &ctx, llvm::Module &module,
           llvm::IRBuilder<> &builder)
      : ctx_(ctx), module_(module), builder_(builder) {}

  llvm::Function *Panic() {
    auto fn = func_map_["__panic"];
    if (!fn) {
      fn = llvm::Function::Create(
          llvm::FunctionType::get(llvm::Type::getVoidTy(ctx_),
                                  {llvm::Type::getInt8PtrTy(ctx_)}, false),
          llvm::GlobalValue::ExternalLinkage, "__panic", module_);
      func_map_["__panic"] = fn;
    }
    return fn;
  }

 private:
  llvm::LLVMContext &ctx_;
  llvm::Module &module_;
  llvm::IRBuilder<> &builder_;

  std::map<std::string, llvm::Function *> func_map_;
};

}  // namespace felis

#endif  // FELIS_INTERNAL_FELIS_LIB_H_
