#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <fstream>
#include <iostream>
#include <string>
#include "common/error.hpp"
#include "parse/lexer.hpp"
#include "parse/printer.hpp"
#include "parse/token.hpp"

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

  Lexer lexer(in, filename);
  Token t;
  while (lexer.next(t)) {
    if (t.kind == TokenKind::LIT_INT) {
      cout << "ival " << t.ival << endl;
    } else if (t.kind == TokenKind::LIT_FLOAT) {
      cout << "fval " << t.fval << endl;
    } else if (t.kind == TokenKind::LIT_BOOL) {
      cout << "bool " << t.bval << endl;
    } else if (t.kind == TokenKind::LIT_STR) {
      cout << "string " << t.sval << endl;
    } else if (t.kind == TokenKind::IDENT) {
      cout << "ident " << t.sval << endl;
    }
  }

/* Lexer lexer(in); */
/* lexer.lex(); */

/* ErrorHandler handler(filename); */
/* auto lexer = make_unique<Lexer>(src); */
/* Parser parser(move(lexer), handler, in, cout); */
/* auto expr = parser.parse(); */
/* if (expr) { */
/*   Printer printer; */
/*   printer.print(expr); */
/* } else { */
/*   handler.report(); */
/* } */

defer:
  in.close();
}

