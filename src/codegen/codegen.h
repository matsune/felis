#ifndef FELIS_CODEGEN_CODEGEN_H_
#define FELIS_CODEGEN_CODEGEN_H_

#include <llvm-c/Core.h>
#include <llvm-c/TargetMachine.h>

#include <string>

namespace felis {

class Codegen {
 public:
  Codegen(std::string moduleName = "felis");
  ~Codegen();
  bool CreateTargetMachine();
  std::string Error() { return error_; }

 private:
  LLVMContextRef ctx_;
  LLVMModuleRef module_;
  LLVMBuilderRef builder_;
  LLVMTargetMachineRef machine_ = nullptr;
  std::string error_;
};

}  // namespace felis

#endif  // FELIS_CODEGEN_CODEGEN_H_
