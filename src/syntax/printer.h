#ifndef FELIS_SYNTAX_PRINTER_H_
#define FELIS_SYNTAX_PRINTER_H_

#include <iostream>
#include <memory>
#include <string>
#include "syntax/ast.h"

namespace felis {

class Printer {
 public:
  void print(std::unique_ptr<File> &);

 private:
  uint16_t depth = 0;
  uint32_t line = 1;
  bool afterNl = false;
  void writeLineNum();
  void indent();
  template <typename... Args>
  void write(const std::string format, Args const &... args);
  template <typename... Args>
  void writeln(const std::string format, Args const &... args);
  void down(std::string);
  void up(std::string);
  void printIdent(Ident *ident);
  void printStmt(Stmt *stmt);
  void printExpr(Expr *expr);
  void printLit(Lit *lit);
  void printBlock(Block *block);
  void printIndex(int idx);
  void printExtern(Extern *);
  void printProto(FnProto *);
  void printFnArg(FnArg *);
  void printFnDecl(FnDecl *fn);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_PRINTER_H_
