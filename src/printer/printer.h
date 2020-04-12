#ifndef FELIS_SYNTAX_PRINTER_H_
#define FELIS_SYNTAX_PRINTER_H_

#include <iostream>
#include <memory>
#include <string>

#include "syntax/ast.h"

namespace felis {

class Printer {
 public:
  void Print(const std::unique_ptr<File> &);

 private:
  uint16_t depth_ = 0;
  uint32_t line_ = 1;
  bool after_nl_ = false;

  void Indent();
  void WriteLineNum();
  template <typename... Args>
  void Write(const std::string format, Args const &... args);
  template <typename... Args>
  void Writeln(const std::string format, Args const &... args);
  void Down(std::string);
  void Up(std::string);
  void PrintIdent(Ident *ident);
  void PrintStmt(Stmt *stmt);
  void PrintExpr(Expr *expr);
  void PrintLit(Lit *lit);
  void PrintBlock(Block *block);
  void PrintIndex(int idx);
  void PrintExtern(Extern *);
  void PrintProto(FnProto *);
  void PrintFnArg(FnArg *);
  void PrintFnDecl(FnDecl *fn);
  void PrintPos(Pos pos);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_PRINTER_H_
