/* #include <llvm/IR/IRBuilder.h> */
/* #include <llvm/IR/LLVMContext.h> */
/* #include <llvm/IR/Module.h> */

#include <fstream>
#include <iostream>
#include <string>

#include "analysis/ty_inferer.h"
#include "printer/printer.h"
#include "syntax/syntax.h"

int error(std::string msg) {
  std::cerr << "felisc: error: " << msg << std::endl;
  return 1;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    return error("no input file");
  }
  std::string filename = argv[1];
  std::ifstream in;
  in.open(filename);
  if (!in.is_open()) {
    return error("failed to open " + filename);
  }

  felis::Parser parser(filename);

  bool isEnd(false);
  felis::Lexer *lexer = new felis::Lexer(in, filename);
  while (!isEnd) {
    auto t = lexer->Next();
    if (lexer->HasError()) {
      error(lexer->Error());
      break;
    }
    isEnd = t->kind == felis::TokenKind::END;
    parser.PushToken(move(t));
  }
  delete lexer;
  in.close();
  if (!isEnd) return 1;

  std::unique_ptr<felis::File> file = parser.Parse();
  if (parser.HasError()) {
    return error(parser.Error());
  }
  /* felis::Printer printer; */
  /* printer.Print(file); */

  felis::TyInferer inferer;
  inferer.Parse(file);
}
