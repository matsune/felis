/* #include <llvm/IR/IRBuilder.h> */
/* #include <llvm/IR/LLVMContext.h> */
/* #include <llvm/IR/Module.h> */
#include <fstream>
#include <iostream>
#include <string>
#include "parse/parser.hpp"

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

  Parser parser(in, cout);
  auto expr = parser.parseExpr();
  cout << expr->kind() << endl;

  in.close();
}

