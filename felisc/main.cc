#include <fstream>
#include <iostream>
#include <string>

#include "args.h"
#include "check/check.h"
#include "error/error.h"
#include "printer/printer.h"
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

  if (opts->printAst) felis::Printer().Print(file);

  felis::Checker checker;
  checker.SetupBuiltin();
  checker.Check(file);

  /* felis::Builder builder; */
  /* std::string err; */
  /* if (!builder.CreateTargetMachine(err)) { */
  /*   std::cerr << opts->filename << ":" << err << std::endl; */
  /*   return 1; */
  /* } */
  /* try { */
  /*   builder.Build(std::move(file)); */
  /* } catch (const felis::CompileError &e) { */
  /*   std::cerr << opts->filename << ":" << e.what() << std::endl; */
  /* } */
  /* try { */
  /*   if (opts->emits & EmitType::LLVM_IR) { */
  /*     builder.EmitLLVMIR(opts->outputName(EmitType::LLVM_IR)); */
  /*   } */
  /*   if (opts->emits & EmitType::LLVM_BC) { */
  /*     builder.EmitLLVMBC(opts->outputName(EmitType::LLVM_BC)); */
  /*   } */
  /*   if (opts->emits & EmitType::ASM) { */
  /*     builder.EmitASM(opts->outputName(EmitType::ASM)); */
  /*   } */
  /*   if (opts->emits & EmitType::OBJ) { */
  /*     builder.EmitOBJ(opts->outputName(EmitType::OBJ)); */
  /*   } */
  /* } catch (std::runtime_error err) { */
  /*   std::cerr << err.what() << std::endl; */
  /*   return 1; */
  /* } */
}
