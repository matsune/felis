#ifndef FELIS_SYNTAX_HIR_PRINTER_H_
#define FELIS_SYNTAX_HIR_PRINTER_H_

#include <iostream>
#include <memory>
#include <string>

#include "node/hir.h"
#include "printer/printer.h"

namespace felis {

class HirPrinter : public Printer<hir::File> {
 public:
  void Print(const std::unique_ptr<hir::File> &) override;

 private:
  void PrintExtern(hir::Extern *);
  void PrintFnDecl(hir::FnDecl *fn);
  void PrintDecl(Decl *);
  void PrintStmt(hir::Stmt *stmt);
  void PrintExpr(hir::Expr *expr);
  void PrintConstant(hir::Constant *);
  void PrintBlock(hir::Block *block);
  void PrintIndex(int idx);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_HIR_PRINTER_H_
