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
  void PrintExtern(ast::Extern *);
  void PrintFunc(ast::Func *);
  void PrintProto(ast::FnProto *);
  void PrintFnArg(ast::FnArg *);
  void PrintBlock(ast::Block *);
  void PrintIdent(ast::Ident *);
  void PrintTypeName(ast::AstNode *);
  void PrintRet(ast::RetStmt *);
  void PrintVarDecl(ast::VarDeclStmt *);
  void PrintAssign(ast::AssignStmt *);
  void PrintStmt(ast::AstNode *);
  void PrintExpr(ast::AstNode *);
  void PrintLit(ast::Literal *);
  void PrintBinary(ast::Binary *);
  void PrintCall(ast::Call *);
  void PrintUnary(ast::Unary *);
  void PrintIf(ast::If *);
  void PrintArray(ast::Array *);
  void PrintIndex(ast::Index *);

  void PrintArrayIndex(int);

  void PrintLoc(ast::AstNode *n) { Writeln("Loc: %d-%d", n->begin, n->end); }
};

}  // namespace felis

#endif  // FELIS_SYNTAX_AST_PRINTER_H_
