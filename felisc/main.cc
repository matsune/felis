#include <fstream>
#include <iostream>
#include <string>

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
    auto lexRes = lexer->Next();
    if (!lexRes) {
      auto posErr = lexRes.UnwrapErr();
      std::cerr << filename << ":" << posErr->what() << std::endl;
      break;
    }
    auto tok = lexRes.Unwrap();
    isEnd = tok->kind == felis::TokenKind::END;
    parser.PushToken(std::unique_ptr<felis::Token>(tok));
  }
  delete lexer;
  in.close();
  if (!isEnd) return 1;

  auto parseRes = parser.Parse();
  if (!parseRes) {
    auto posErr = parseRes.UnwrapErr();
    std::cerr << filename << ":" << posErr->what() << std::endl;
    return 1;
  }
  auto file = std::unique_ptr<felis::File>(parseRes.Unwrap());
  felis::Printer printer;
  printer.Print(file);

  felis::Builder builder;
  std::string err;
  if (!builder.CreateTargetMachine(err)) {
    std::cerr << filename << ":" << err << std::endl;
    return 1;
  }
  /* if (!builder.Build(std::move(file))) { */
  /*   handler.Report(); */
  /*   return 1; */
  /* } */
}
