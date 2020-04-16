#include <fstream>
#include <iostream>
#include <string>

#include "args.h"
#include "error/error.h"
#include "ir/builder.h"
#include "printer/printer.h"
#include "syntax/lexer.h"
#include "syntax/parser.h"

std::unique_ptr<felis::File> ParseFile(std::ifstream &in) {
  felis::Parser parser;
  felis::Lexer lexer(in);
  bool isEnd(false);
  while (!isEnd) {
    auto token = lexer.Next();
    isEnd = token->kind == felis::TokenKind::END;
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

  std::unique_ptr<felis::File> file;
  try {
    file = ParseFile(in);
  } catch (const felis::CompileError &e) {
    std::cerr << opts->filename << ":" << e.what() << std::endl;
    in.close();
    return 1;
  }
  in.close();

  if (opts->printAst) felis::Printer().Print(file);

  felis::Builder builder;
  std::string err;
  if (!builder.CreateTargetMachine(err)) {
    std::cerr << opts->filename << ":" << err << std::endl;
    return 1;
  }
  try {
    builder.Build(std::move(file));
  } catch (const felis::CompileError &e) {
    std::cerr << opts->filename << ":" << e.what() << std::endl;
  }
}
