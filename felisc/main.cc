#include <fstream>
#include <iostream>
#include <string>

#include "analysis/ty_inferer.h"
#include "codegen/codegen.h"
#include "printer/printer.h"
#include "syntax/syntax.h"

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

  felis::ErrorHandler handler(filename);
  felis::Parser parser(handler);

  bool isEnd(false);
  felis::Lexer *lexer = new felis::Lexer(in, handler);
  while (!isEnd) {
    auto t = lexer->Next();
    if (handler.HasError()) {
      handler.Report();
      break;
    }
    isEnd = t->kind == felis::TokenKind::END;
    parser.PushToken(move(t));
  }
  delete lexer;
  in.close();
  if (!isEnd) return 1;

  std::unique_ptr<felis::File> file = parser.Parse();
  if (handler.HasError()) {
    handler.Report();
    return 1;
  }

  felis::Printer printer;
  printer.Print(file);

  felis::TyInferer inferer(handler);
  inferer.Parse(file);
  if (handler.HasError()) {
    handler.Report();
    return 1;
  }

  felis::Codegen gen;
  if (!gen.CreateTargetMachine()) {
    std::cerr << gen.Error() << std::endl;
    return 1;
  }
}
