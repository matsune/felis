#ifndef FELIS_SYNTAX_AST_PRINTER_H_
#define FELIS_SYNTAX_AST_PRINTER_H_

#include <iostream>
#include <memory>
#include <string>

#include "node/ast.h"
#include "printer/printer.h"

namespace felis {

class AstPrinter : public Printer<ast::File> {
 public:
  void Print(const std::unique_ptr<ast::File> &) override;

 private:
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
};

}  // namespace felis

#endif  // FELIS_SYNTAX_AST_PRINTER_H_
