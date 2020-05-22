#include <cxxopts.hpp>
#include <memory>
#include <string>

#include "opts.h"

std::unique_ptr<Opts> ParseArgs(int argc, char *argv[]) {
  cxxopts::Options options("felisc");
  options.positional_help("INPUT");
  // clang-format off
  options.add_options()
    ("h,help",  "Print this message")
    ("ast",   "Print AST tree", cxxopts::value<bool>()->default_value("false"))
    ("hir",   "Print HIR tree", cxxopts::value<bool>()->default_value("false"))
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

  return std::make_unique<Opts>(
      input.as<std::string>(), result["out"].as<std::string>(),
      result["ast"].as<bool>(), result["hir"].as<bool>(), emit);
}

