#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <fstream>
#include <iostream>
#include <parse/error.hpp>
#include <parse/parser.hpp>
#include <parse/source.hpp>
#include <string>

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

  ErrorHandler handler(filename);
  Source src(filename);
  auto lexer = make_unique<Lexer>(src);
  Parser parser(move(lexer), handler, in, cout);
  auto expr = parser.parse();
  if (handler.report()) {
    goto defer;
  }
  if (expr) cout << expr->kind() << endl;

defer:
  in.close();
}

