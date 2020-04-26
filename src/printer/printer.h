#ifndef FELIS_SYNTAX_PRINTER_H_
#define FELIS_SYNTAX_PRINTER_H_

#include <iostream>
#include <memory>
#include <string>

#include "syntax/ast.h"

namespace felis {

class Printer {
 public:
  void Print(const std::unique_ptr<ast::File> &);

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
  void PrintIdent(ast::Ident *ident);
  void PrintStmt(ast::Stmt *stmt);
  void PrintExpr(ast::Expr *expr);
  void PrintLit(ast::Lit *lit);
  void PrintBlock(ast::Block *block);
  void PrintIndex(int idx);
  void PrintExtern(ast::Extern *);
  void PrintProto(ast::FnProto *);
  void PrintFnArg(ast::FnArg *);
  void PrintFnDecl(ast::FnDecl *fn);
  void PrintPos(Pos pos);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_PRINTER_H_
