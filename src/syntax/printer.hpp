#ifndef PRINTER_HPP
#define PRINTER_HPP

#include <iostream>
#include <memory>
#include "ast.hpp"

class Printer {
  uint16_t depth = 0;
  uint32_t line = 1;
  bool afterNl = false;
  void writeLineNum();
  void indent();
  template <typename... Args>
  void write(const string format, Args const &... args);
  template <typename... Args>
  void writeln(const string format, Args const &... args);
  void down(string);
  void up(string);
  void printBlock(Block *block);
  void printIdent(Ident *ident);
  void printStmt(Stmt *stmt);
  void printExpr(Expr *expr);
  void printLit(Lit *lit);

 public:
  void print(unique_ptr<File> &);
};

#endif
