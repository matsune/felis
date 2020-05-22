#ifndef FELIS_CHECK_TY_INFER_H_
#define FELIS_CHECK_TY_INFER_H_

#include <map>
#include <memory>

#include "check/node_map.h"
#include "node/ast.h"
#include "node/hir.h"

namespace felis {

class TyInfer {
 public:
  TyInfer(NodeMap &node_map) : node_map_(node_map){};
  void Infer(std::unique_ptr<ast::File> &);

 private:
  NodeMap &node_map_;
  std::shared_ptr<FuncType> current_func_;

  std::shared_ptr<Decl> GetDecl(std::unique_ptr<ast::Ident> &t) {
    return node_map_.GetDecl(t);
  }

  template <typename T>
  std::shared_ptr<Ty> RecordType(const std::unique_ptr<T> &n,
                                 std::shared_ptr<Ty> ty) {
    return node_map_.RecordType(n, ty);
  }

  std::shared_ptr<Ty> InferStmt(const std::unique_ptr<ast::Stmt> &,
                                bool as_value);
  std::shared_ptr<Ty> InferExpr(const std::unique_ptr<ast::Expr> &,
                                bool as_value);
  void InferRet(const std::unique_ptr<ast::RetStmt> &);
  void InferVarDecl(const std::unique_ptr<ast::VarDeclStmt> &);
  void InferAssign(const std::unique_ptr<ast::AssignStmt> &);
  std::shared_ptr<Ty> InferIf(const std::unique_ptr<ast::If> &, bool as_value);
  std::shared_ptr<Ty> InferBlock(const std::unique_ptr<ast::Block> &,
                                 bool as_value);
};

}  // namespace felis

#endif  // FELIS_CHECK_TY_INFER_H_
