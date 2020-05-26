#include "builder/target.h"

#include <llvm/ADT/StringMap.h>
#include <llvm/MC/SubtargetFeature.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetOptions.h>

#include <iostream>

namespace felis {

bool is_initialized_llvm = false;

void InitilizeLLVM() {
  if (is_initialized_llvm) return;
  is_initialized_llvm = true;

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllDisassemblers();
}

std::string GetHostCPUFeatures() {
  llvm::SubtargetFeatures Features;
  llvm::StringMap<bool> HostFeatures;

  if (llvm::sys::getHostCPUFeatures(HostFeatures))
    for (auto &F : HostFeatures) Features.AddFeature(F.first(), F.second);

  return Features.getString();
}

std::unique_ptr<llvm::TargetMachine> CreateHostTargetMachine(std::string &err) {
  InitilizeLLVM();

  std::string triple = llvm::sys::getDefaultTargetTriple();
  std::string cpu = llvm::sys::getHostCPUName();
  std::string features = GetHostCPUFeatures();
  const llvm::Target *target = llvm::TargetRegistry::lookupTarget(triple, err);
  if (!target) {
    return nullptr;
  }

  llvm::TargetOptions opt;
  return std::unique_ptr<llvm::TargetMachine>(
      target->createTargetMachine(triple, cpu, features, opt, llvm::None));
}

std::unique_ptr<llvm::TargetMachine> CreateTargetMachine(
    const std::string &triple, std::string &err) {
  InitilizeLLVM();

  if (triple.empty()) {
    return CreateHostTargetMachine(err);
  } else {
    const llvm::Target *target =
        llvm::TargetRegistry::lookupTarget(triple, err);
    if (!target) {
      return nullptr;
    }

    llvm::TargetOptions opt;
    return std::unique_ptr<llvm::TargetMachine>(
        target->createTargetMachine(triple, "", "", opt, llvm::None));
  }
}

}  // namespace felis
