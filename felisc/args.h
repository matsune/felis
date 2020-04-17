#include <cxxopts.hpp>
#include <memory>
#include <string>

#include "ir/builder.h"

struct Opts {
  Opts(std::string filename, bool printAst, felis::Emits emits)
      : filename(filename), printAst(printAst), emits(emits){};
  std::string filename;
  bool printAst;
  felis::Emits emits;
};

std::unique_ptr<Opts> ParseArgs(int argc, char *argv[]) {
  cxxopts::Options options("felisc");
  options.positional_help("INPUT");
  options.add_options()("h,help", "Print this message")(
      "a,ast", "Print AST tree",
      cxxopts::value<bool>()->default_value("false"))(
      "emit",
      "[llvm-ir|llvm-bc|asm|obj|link] types of output for the compiler to "
      "emit",
      cxxopts::value<std::vector<std::string>>()
          ->implicit_value({"link"})
          ->default_value("link"))("INPUT", "Input file",
                                   cxxopts::value<std::string>());
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
  uint8_t emit;
  for (auto &e : result["emit"].as<std::vector<std::string>>()) {
    if (e == "llvm-ir") {
      emit |= felis::EmitType::LLVM_IR;
    } else if (e == "llvm-bc") {
      emit |= felis::EmitType::LLVM_BC;
    } else if (e == "asm") {
      emit |= felis::EmitType::ASM;
    } else if (e == "obj") {
      emit |= felis::EmitType::OBJ;
    } else if (e == "link") {
      emit |= felis::EmitType::LINK;
    } else {
      std::cerr << "unknown emit flag" << std::endl;
      exit(1);
    }
  }
  return std::make_unique<Opts>(input.as<std::string>(),
                                result["ast"].as<bool>(), emit);
}

