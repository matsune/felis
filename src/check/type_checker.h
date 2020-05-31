#ifndef FELIS_CHECK_TYPE_CHECKER_H_
#define FELIS_CHECK_TYPE_CHECKER_H_

#include <map>
#include <memory>

#include "check/ctx.h"
#include "check/decl_checker.h"
#include "node/ast.h"

namespace felis {

class TypeChecker {
 public:
  TypeChecker(TypeCheckCtx &ctx) : ctx_(ctx), decl_ck_(ctx.Is32bit()) {}

  void Check(const std::unique_ptr<ast::File> &);

 private:
  TypeCheckCtx &ctx_;
  DeclChecker decl_ck_;
  std::shared_ptr<FuncType> current_func_;

  std::shared_ptr<Type> InferStmt(const std::unique_ptr<ast::Stmt> &);
  void InferRet(const std::unique_ptr<ast::RetStmt> &);
  void InferVarDecl(const std::unique_ptr<ast::VarDeclStmt> &);
  void InferAssign(const std::unique_ptr<ast::AssignStmt> &);
  std::shared_ptr<Type> InferExpr(const std::unique_ptr<ast::Expr> &);
  std::shared_ptr<Type> InferIf(const std::unique_ptr<ast::If> &);
  std::shared_ptr<Type> InferBlock(const std::unique_ptr<ast::Block> &);
};

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_CHECKER_H_
