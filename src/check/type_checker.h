#ifndef FELIS_CHECK_TYPE_CHECKER_H_
#define FELIS_CHECK_TYPE_CHECKER_H_

#include <map>
#include <memory>

#include "check/decl_checker.h"
#include "node/ast.h"

namespace felis {

using IdentDeclMap = std::map<const ast::Ident *, std::shared_ptr<Decl>>;
using ExprTypeMap = std::map<const ast::Expr *, std::shared_ptr<Type>>;

class TypeChecker {
 public:
  TypeChecker(bool is_32bit, IdentDeclMap &ident_decl_map,
              ExprTypeMap &expr_type_map)
      : is_32bit(is_32bit),
        ident_decl_map_(ident_decl_map),
        expr_type_map_(expr_type_map),
        decl_ck(is_32bit) {}

  void Check(const std::unique_ptr<ast::File> &);

 private:
  bool is_32bit;
  IdentDeclMap &ident_decl_map_;
  ExprTypeMap &expr_type_map_;
  DeclChecker decl_ck;
  std::shared_ptr<FuncType> current_func_;

  std::shared_ptr<Type> InferStmt(const std::unique_ptr<ast::Stmt> &);
  void InferRet(const std::unique_ptr<ast::RetStmt> &);
  void InferVarDecl(const std::unique_ptr<ast::VarDeclStmt> &);
  void InferAssign(const std::unique_ptr<ast::AssignStmt> &);
  std::shared_ptr<Type> InferExpr(const std::unique_ptr<ast::Expr> &);
  std::shared_ptr<Type> InferIf(const std::unique_ptr<ast::If> &);
  std::shared_ptr<Type> InferBlock(const std::unique_ptr<ast::Block> &);

  template <typename T>
  void RecordDecl(const std::unique_ptr<T> &n, std::shared_ptr<Decl> ty) {
    ident_decl_map_[n.get()] = ty;
  }

  template <typename T>
  std::shared_ptr<Type> RecordType(const std::unique_ptr<T> &n,
                                   std::shared_ptr<Type> ty) {
    std::cout << "RecordType " << n.get() << " " << ToString(ty) << std::endl;
    expr_type_map_[n.get()] = ty;
    return ty;
  }
};

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_CHECKER_H_
