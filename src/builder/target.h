#ifndef FELIS_BUILDER_TARGET_H_
#define FELIS_BUILDER_TARGET_H_

#include <llvm/Target/TargetMachine.h>

#include <memory>
#include <string>

namespace felis {

std::unique_ptr<llvm::TargetMachine> CreateTargetMachine(const std::string &,
                                                         std::string &);

}  // namespace felis

#endif  // FELIS_BUILDER_TARGET_H_
