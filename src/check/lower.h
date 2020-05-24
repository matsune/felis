#ifndef FELIS_CHECK_LOWER_H_
#define FELIS_CHECK_LOWER_H_

#include <map>
#include <memory>

#include "check/decl_checker.h"
#include "check/ty_infer.h"
#include "node/ast.h"
#include "node/hir.h"

namespace felis {

class Lower {
 public:
  Lower()
      : decl_checker_(DeclChecker(node_map_)), ty_infer_(TyInfer(node_map_)){};

  std::unique_ptr<hir::File> Lowering(std::unique_ptr<ast::File>,
                                      bool is_32bit);

 private:
  NodeMap node_map_;
  DeclChecker decl_checker_;
  TyInfer ty_infer_;

  std::unique_ptr<hir::Stmt> LowerStmt(std::unique_ptr<ast::Stmt>);
  std::unique_ptr<hir::RetStmt> LowerRet(std::unique_ptr<ast::RetStmt>);
  std::unique_ptr<hir::VarDeclStmt> LowerVarDecl(
      std::unique_ptr<ast::VarDeclStmt>);
  std::unique_ptr<hir::AssignStmt> LowerAssign(
      std::unique_ptr<ast::AssignStmt>);
  std::unique_ptr<hir::Expr> LowerExpr(std::unique_ptr<ast::Expr>);
  std::unique_ptr<hir::Block> LowerBlock(std::unique_ptr<ast::Block>);
  std::unique_ptr<hir::If> LowerIf(std::unique_ptr<ast::If>);

  std::unique_ptr<hir::Value> LowerLit(std::unique_ptr<ast::Lit>);
  std::unique_ptr<hir::Value> ParseInt(std::unique_ptr<ast::Lit>);
  std::unique_ptr<hir::FloatConstant> ParseFloat(std::unique_ptr<ast::Lit>);
};

}  // namespace felis

#endif  // FELIS_CHECK_LOWER_H_
