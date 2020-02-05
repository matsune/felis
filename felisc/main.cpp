#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <fstream>
#include <iostream>
#include <string>
#include "syntax/syntax.hpp"

using namespace std;

int main(int argc, char *argv[]) {
  if (argc < 2) {
    cerr << "passing no file" << endl;
    return 1;
  }
  string filename = argv[1];
  ifstream in;
  in.open(filename);
  if (!in.is_open()) {
    cerr << "failed to open " << filename << endl;
    return 1;
  }

  Parser parser(filename);

  bool isEnd(false);
  Lexer *lexer = new Lexer(in, filename);
  while (!isEnd) {
    auto t = make_unique<Token>();
    if (!lexer->next(t)) {
      break;
    }
    isEnd = t->kind == TokenKind::END;
    parser.push_token(move(t));
  }
  delete lexer;
  in.close();
  if (!isEnd) return 1;

  try {
    auto file = parser.parse();
    if (file) {
      Printer printer;
      printer.print(file);
    }
  } catch (const FatalError &e) {
    cerr << e.msg << endl;
  }
}

