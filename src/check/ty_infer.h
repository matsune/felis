#ifndef FELIS_CHECK_TY_INFER_H_
#define FELIS_CHECK_TY_INFER_H_

#include <map>
#include <memory>

#include "node/ast.h"
#include "node/hir.h"

namespace felis {

class TyInfer {
 public:
  TyInfer(std::map<ast::AstNode *, std::shared_ptr<Decl>> &ast_decl)
      : ast_decl_(ast_decl){};
  void Infer(const std::unique_ptr<ast::File> &);
  std::map<const ast::AstNode *, std::shared_ptr<Type>> ty_map;

 private:
  std::map<ast::AstNode *, std::shared_ptr<Decl>> &ast_decl_;
  FuncType *current_func_;

  void MakeType(const ast::AstNode *n, std::shared_ptr<Type> ty) {
    ty_map[n] = ty;
  }
  std::shared_ptr<Type> InferStmt(const std::unique_ptr<ast::Stmt> &);
  std::shared_ptr<Type> InferExpr(const std::unique_ptr<ast::Expr> &);
  void InferRet(const std::unique_ptr<ast::RetStmt> &);
  void InferVarDecl(const std::unique_ptr<ast::VarDeclStmt> &);
  void InferAssign(const std::unique_ptr<ast::AssignStmt> &);
  std::shared_ptr<Type> InferIf(const std::unique_ptr<ast::If> &);
  std::shared_ptr<Type> InferBlock(const std::unique_ptr<ast::Block> &);
};

}  // namespace felis

#endif  // FELIS_CHECK_TY_INFER_H_
