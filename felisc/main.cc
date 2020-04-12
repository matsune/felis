#include <fstream>
#include <iostream>
#include <string>

#include "error/error.h"
#include "ir/builder.h"
#include "printer/printer.h"
#include "syntax/lexer.h"
#include "syntax/parser.h"

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cerr << "felisc: no input file" << std::endl;
    return 1;
  }
  std::string filename = argv[1];
  std::ifstream in;
  in.open(filename);
  if (!in.is_open()) {
    std::cerr << "felisc: failed to open " << filename << std::endl;
    return 1;
  }

  felis::Parser parser;

  bool isEnd(false);
  felis::Lexer *lexer = new felis::Lexer(in);
  while (!isEnd) {
    try {
      auto token = lexer->Next();
      isEnd = token->kind == felis::TokenKind::END;
      parser.PushToken(std::move(token));
    } catch (const felis::CompileError &e) {
      std::cerr << filename << ":" << e.what() << std::endl;
      break;
    }
  }
  delete lexer;
  in.close();
  if (!isEnd) return 1;

  std::unique_ptr<felis::File> file;
  try {
    file = parser.Parse();
  } catch (const felis::CompileError &e) {
    std::cerr << filename << ":" << e.what() << std::endl;
    return 1;
  }

  felis::Printer printer;
  printer.Print(file);

  felis::Builder builder;
  std::string err;
  if (!builder.CreateTargetMachine(err)) {
    std::cerr << filename << ":" << err << std::endl;
    return 1;
  }
  try {
    builder.Build(std::move(file));
  } catch (const felis::CompileError &e) {
    std::cerr << filename << ":" << e.what() << std::endl;
  }
}
