#ifndef FELIS_CHECK_TY_INFER_H_
#define FELIS_CHECK_TY_INFER_H_

#include <map>
#include <memory>

#include "node/ast.h"
#include "node/hir.h"

namespace felis {

class TyInfer {
  using IdentDeclMap = std::map<ast::Ident *, std::shared_ptr<Decl>>;
  using NodeTyMap = std::map<const ast::AstNode *, std::shared_ptr<Ty>>;

 public:
  TyInfer(std::map<ast::Ident *, std::shared_ptr<Decl>> &decl_map)
      : decl_map_(decl_map){};
  void Infer(std::unique_ptr<ast::File> &);

 private:
  IdentDeclMap &decl_map_;
  NodeTyMap ty_map;
  std::shared_ptr<FuncType> current_func_;

  std::shared_ptr<Decl> GetDecl(std::unique_ptr<ast::Ident> &t) {
    return decl_map_.at(t.get());
  }

  template <typename T>
  std::shared_ptr<Ty> RecordType(const std::unique_ptr<T> &n,
                                 std::shared_ptr<Ty> ty) {
    ty_map[n.get()] = ty;
    return ty;
  }

  std::shared_ptr<Ty> InferStmt(const std::unique_ptr<ast::Stmt> &);
  std::shared_ptr<Ty> InferExpr(const std::unique_ptr<ast::Expr> &);
  void InferRet(const std::unique_ptr<ast::RetStmt> &);
  void InferVarDecl(const std::unique_ptr<ast::VarDeclStmt> &);
  void InferAssign(const std::unique_ptr<ast::AssignStmt> &);
  std::shared_ptr<Ty> InferIf(const std::unique_ptr<ast::If> &);
  std::shared_ptr<Ty> InferBlock(const std::unique_ptr<ast::Block> &);
};

}  // namespace felis

#endif  // FELIS_CHECK_TY_INFER_H_
