#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <fstream>
#include <iostream>
#include <string>
#include "syntax/syntax.h"

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cerr << "passing no file" << std::endl;
    return 1;
  }
  std::string filename = argv[1];
  std::ifstream in;
  in.open(filename);
  if (!in.is_open()) {
    std::cerr << "failed to open " << filename << std::endl;
    return 1;
  }

  felis::Parser parser(filename);

  bool isEnd(false);
  felis::Lexer *lexer = new felis::Lexer(in, filename);
  while (!isEnd) {
    auto t = std::make_unique<felis::Token>();
    if (!lexer->next(t)) {
      break;
    }
    isEnd = t->kind == felis::TokenKind::END;
    parser.push_token(move(t));
  }
  delete lexer;
  in.close();
  if (!isEnd) return 1;

  try {
    auto file = parser.parse();
    if (file) {
      felis::Printer printer;
      printer.print(file);
    }
  } catch (const felis::FatalError &e) {
    std::cerr << e.msg << std::endl;
  }
}

