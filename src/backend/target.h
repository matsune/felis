#ifndef FELIS_BACKEND_TARGET_H_
#define FELIS_BACKEND_TARGET_H_

#include <llvm/Target/TargetMachine.h>

#include <memory>
#include <string>

namespace felis {

std::unique_ptr<llvm::TargetMachine> CreateTargetMachine(const std::string &,
                                                         std::string &);

}  // namespace felis

#endif  // FELIS_BACKEND_TARGET_H_
