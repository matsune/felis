#include "codegen.h"

#include <llvm-c/Target.h>

#include <iostream>

namespace felis {

Codegen::Codegen(std::string moduleName) {
  ctx_ = LLVMContextCreate();
  module_ = LLVMModuleCreateWithNameInContext(moduleName.c_str(), ctx_);
  builder_ = LLVMCreateBuilderInContext(ctx_);
};

Codegen::~Codegen() {
  if (machine_) LLVMDisposeTargetMachine(machine_);
  LLVMDisposeBuilder(builder_);
  LLVMDisposeModule(module_);
  LLVMContextDispose(ctx_);
};

bool Codegen::CreateTargetMachine() {
  LLVMInitializeNativeTarget();

  char *triple = LLVMGetDefaultTargetTriple();
  char *cpuName = LLVMGetHostCPUName();
  char *features = LLVMGetHostCPUFeatures();
  char *error;
  LLVMTargetRef target;
  if (LLVMGetTargetFromTriple(triple, &target, &error)) {
    error_ = error;
    return false;
  }
  machine_ = LLVMCreateTargetMachine(target, triple, cpuName, features,
                                     LLVMCodeGenLevelDefault, LLVMRelocDefault,
                                     LLVMCodeModelDefault);
  return true;
}

}  // namespace felis
