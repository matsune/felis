#include <cxxopts.hpp>
#include <memory>
#include <string>

#include "ir/builder.h"

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

struct Opts {
  Opts(std::string filename, std::string output, bool printAst, Emits emits,
       bool isMultiEmits)
      : filename(filename),
        output(output),
        printAst(printAst),
        emits(emits),
        isMultiEmits(isMultiEmits){};
  std::string filename;
  std::string output;
  bool printAst;
  Emits emits;
  bool isMultiEmits;

  std::string outputName(EmitType emit) {
    assert(emits & emit);
    if (output.empty()) {
      return fileStem(filename) + extOfEmit(emit);
    }
    if (isMultiEmits) {
      return fileStem(output) + extOfEmit(emit);
    } else {
      return output;
    }
  }
};

std::unique_ptr<Opts> ParseArgs(int argc, char *argv[]) {
  cxxopts::Options options("felisc");
  options.positional_help("INPUT");
  // clang-format off
  options.add_options()
    ("h,help",  "Print this message")
    ("a,ast",   "Print AST tree", cxxopts::value<bool>()->default_value("false"))
    ("o,out",   "Write output to to <FILENAME>", cxxopts::value<std::string>()->default_value(""))
    ("emit",    "[llvm-ir|llvm-bc|asm|obj|link] types of output for the compiler to ", 
     cxxopts::value<std::vector<std::string>>()->implicit_value({"link"})->default_value("link"))
    ("INPUT",   "Input file", cxxopts::value<std::string>());
  // clang-format on
  options.parse_positional("INPUT");
  auto result = options.parse(argc, argv);
  if (result.count("help")) {
    std::cout << options.help() << std::endl;
    exit(0);
  }
  auto input = result["INPUT"];
  if (!input.count()) {
    std::cerr << "felisc: no input file" << std::endl;
    exit(1);
  }
  auto emits = result["emit"].as<std::vector<std::string>>();
  uint8_t emit(0);
  for (auto &e : emits) {
    if (e == "llvm-ir") {
      emit |= EmitType::LLVM_IR;
    } else if (e == "llvm-bc") {
      emit |= EmitType::LLVM_BC;
    } else if (e == "asm") {
      emit |= EmitType::ASM;
    } else if (e == "obj") {
      emit |= EmitType::OBJ;
    } else if (e == "link") {
      emit |= EmitType::LINK;
    } else {
      std::cerr << "unknown emit flag" << std::endl;
      exit(1);
    }
  }

  bool isMultiEmits = emits.size() > 1;
  bool hasOut = result.count("out") > 0;
  if (isMultiEmits && hasOut) {
    std::cerr
        << "due to multiple output types requested, the explicitly specified "
           "output file name will be adapted for each output type"
        << std::endl;
  }

  return std::make_unique<Opts>(input.as<std::string>(),
                                result["out"].as<std::string>(),
                                result["ast"].as<bool>(), emit, isMultiEmits);
}

