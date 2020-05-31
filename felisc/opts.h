#include <string>

enum EmitType {
  LINK = (1u << 0),
  LLVM_IR = (1u << 1),
  LLVM_BC = (1u << 2),
  ASM = (1u << 3),
  OBJ = (1u << 4),
};

using Emits = uint8_t;

std::string fileBase(std::string path) {
  if (path == "") {
    return ".";
  }
  while (!path.empty() && (path[path.size() - 1] == '/')) {
    path = path.substr(0, path.size() - 1);
  }
  path = path.substr(path.rfind("/") + 1);
  if (path == "") {
    return "/";
  }
  return path;
}

std::string fileStem(std::string path) {
  path = fileBase(path);
  return path.substr(0, path.rfind("."));
}

std::string extOfEmit(EmitType emit) {
  switch (emit) {
    case EmitType::LLVM_IR:
      return ".ll";
    case EmitType::LLVM_BC:
      return ".bc";
    case EmitType::ASM:
      return ".s";
    case EmitType::OBJ:
      return ".o";
    case EmitType::LINK:
      return "";
  }
}

class Opts {
 public:
  Opts(std::string filepath, std::string output, std::string target,
       bool print_ast, bool print_mir, Emits emits)
      : filepath_(filepath),
        output_(output),
        target_(target),
        print_ast_(print_ast),
        print_mir_(print_mir),
        emits_(emits){};

  std::string OutputName(EmitType emit) {
    if (output_.empty()) {
      return fileStem(filepath_) + extOfEmit(emit);
    }
    if (IsMultiEmits()) {
      return fileStem(output_) + extOfEmit(emit);
    } else {
      return output_;
    }
  }

  std::string &Filepath() { return filepath_; }

  std::string &Target() { return target_; }

  bool IsPrintAst() { return print_ast_; }

  bool IsPrintMir() { return print_mir_; }

  bool IsMultiEmits() {
    return emits_ != 0 && emits_ != EmitType::LINK &&
           emits_ != EmitType::LLVM_IR && emits_ != EmitType::LLVM_BC &&
           emits_ != EmitType::ASM && emits_ != EmitType::OBJ;
  }

  bool IsEmit(EmitType emit) { return emits_ & emit; }

 private:
  std::string filepath_;
  std::string output_;
  std::string target_;
  bool print_ast_;
  bool print_mir_;
  Emits emits_;
};

