#include <llvm/ADT/StringMap.h>
#include <llvm/MC/SubtargetFeature.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>

#include <fstream>
#include <iostream>
#include <string>

#include "args.h"
#include "builder/builder.h"
#include "check/check.h"
#include "error/error.h"
#include "printer/ast_printer.h"
#include "printer/hir_printer.h"
#include "syntax/lexer.h"
#include "syntax/parser.h"

std::unique_ptr<felis::ast::File> ParseFile(std::ifstream &in) {
  felis::Parser parser;
  felis::Lexer lexer(in);
  bool isEnd(false);
  while (!isEnd) {
    auto token = lexer.Next();
    isEnd = token->kind == felis::Token::Kind::END;
    parser.PushToken(std::move(token));
  }
  return parser.Parse();
}

std::string getHostCPUFeatures() {
  llvm::SubtargetFeatures Features;
  llvm::StringMap<bool> HostFeatures;

  if (llvm::sys::getHostCPUFeatures(HostFeatures))
    for (auto &F : HostFeatures) Features.AddFeature(F.first(), F.second);

  return Features.getString();
}

std::unique_ptr<llvm::TargetMachine> CreateTargetMachine(std::string &err) {
  if (llvm::InitializeNativeTarget()) return nullptr;
  if (llvm::InitializeNativeTargetAsmPrinter()) return nullptr;

  std::string triple = llvm::sys::getDefaultTargetTriple();
  std::string cpu = llvm::sys::getHostCPUName();
  std::string features = getHostCPUFeatures();
  const llvm::Target *target = llvm::TargetRegistry::lookupTarget(triple, err);
  if (!target) {
    return nullptr;
  }

  llvm::TargetOptions opt;
  return std::unique_ptr<llvm::TargetMachine>(
      target->createTargetMachine(triple, cpu, features, opt, llvm::None));
}

int main(int argc, char *argv[]) {
  auto opts = ParseArgs(argc, argv);
  std::ifstream in;
  in.open(opts->filename);
  if (!in.is_open()) {
    std::cerr << "felisc: failed to open " << opts->filename << std::endl;
    return 1;
  }

  std::unique_ptr<felis::ast::File> file;
  try {
    file = ParseFile(in);
  } catch (const felis::CompileError &e) {
    std::cerr << opts->filename << ":" << e.what() << std::endl;
    in.close();
    return 1;
  }
  in.close();

  if (opts->printAst) felis::AstPrinter().Print(file);

  felis::Checker checker;
  checker.SetupBuiltin();
  std::unique_ptr<felis::hir::File> hir = checker.Check(std::move(file));

  felis::HirPrinter().Print(hir);

  std::string err;
  auto machine = CreateTargetMachine(err);
  if (!machine) {
    std::cerr << opts->filename << ":" << err << std::endl;
    return 1;
  }
  felis::Builder builder("felis", opts->filename, std::move(machine));
  try {
    builder.Build(std::move(hir));
  } catch (const felis::CompileError &e) {
    std::cerr << opts->filename << ":" << e.what() << std::endl;
  }
  try {
    if (opts->emits & EmitType::LLVM_IR) {
      builder.EmitLLVMIR(opts->outputName(EmitType::LLVM_IR));
    }
    if (opts->emits & EmitType::LLVM_BC) {
      builder.EmitLLVMBC(opts->outputName(EmitType::LLVM_BC));
    }
    if (opts->emits & EmitType::ASM) {
      builder.EmitASM(opts->outputName(EmitType::ASM));
    }
    bool emitObj = opts->emits & EmitType::OBJ;
    bool emitLink = opts->emits & EmitType::LINK;

    bool hasObj = false;
    std::string objPath = opts->outputName(EmitType::OBJ);
    if (emitObj || emitLink) {
      builder.EmitOBJ(objPath);
      hasObj = true;
    }
    if (emitLink) {
      std::string out = opts->outputName(EmitType::LINK);
      std::string s = "gcc " + objPath + " -o " + out;
      system(s.c_str());
    }
  } catch (std::runtime_error err) {
    std::cerr << err.what() << std::endl;
    return 1;
  }
}
