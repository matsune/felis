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
  void PrintIndex(int idx);
  void PrintExtern(hir::Extern *);
  void PrintFnDecl(hir::FnDecl *fn);
  void PrintBlock(hir::Block *block);
  void PrintStmt(hir::Stmt *stmt);
  void PrintExpr(hir::Expr *expr);
};

}  // namespace felis

#endif  // FELIS_SYNTAX_HIR_PRINTER_H_
