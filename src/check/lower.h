//#ifndef FELIS_CHECK_LOWER_H_
//#define FELIS_CHECK_LOWER_H_
//
//#include <map>
//#include <memory>
//
//#include "check/type_checker.h"
//#include "node/ast.h"
//#include "node/hir.h"
//
// namespace felis {
//
// class Lower {
// public:
//  Lower(IdentDeclMap& ident_decl_map, ExprTypeMap& expr_type_map)
//      : ident_decl_map_(ident_decl_map), expr_type_map_(expr_type_map) {}
//
//  std::unique_ptr<hir::File> Lowering(std::unique_ptr<ast::File>);
//
// private:
//  const IdentDeclMap& ident_decl_map_;
//  const ExprTypeMap& expr_type_map_;
//
//  auto& GetDecl(const std::unique_ptr<ast::Ident>& t) const {
//    std::cout << "GetDecl " << t.get() << std::endl;
//    return ident_decl_map_.at(t.get());
//  }
//
//  template <typename K>
//  std::shared_ptr<FixedType> GetType(const std::unique_ptr<K>& n) const {
//    std::cout << "GetType of expr " << n.get() << std::endl;
//    return std::dynamic_pointer_cast<FixedType>(expr_type_map_.at(n.get()));
//  }
//
//  std::unique_ptr<hir::Stmt> LowerStmt(std::unique_ptr<ast::Stmt>);
//  std::unique_ptr<hir::RetStmt> LowerRet(std::unique_ptr<ast::RetStmt>);
//  std::unique_ptr<hir::VarDeclStmt> LowerVarDecl(
//      std::unique_ptr<ast::VarDeclStmt>);
//  std::unique_ptr<hir::AssignStmt> LowerAssign(
//      std::unique_ptr<ast::AssignStmt>);
//  std::unique_ptr<hir::Expr> LowerExpr(std::unique_ptr<ast::Expr>);
//  std::unique_ptr<hir::Block> LowerBlock(std::unique_ptr<ast::Block>);
//  std::unique_ptr<hir::If> LowerIf(std::unique_ptr<ast::If>);
//
//  std::unique_ptr<hir::Value> LowerLit(std::unique_ptr<ast::Lit>);
//  std::unique_ptr<hir::Value> ParseIntLit(std::unique_ptr<ast::Lit>);
//  std::unique_ptr<hir::FloatConstant>
//  ParseFloatLit(std::unique_ptr<ast::Lit>);
//};
//
// std::unique_ptr<hir::File> Lowering(std::unique_ptr<ast::File>, bool
// is_32bit);
//
//}  // namespace felis
//
//#endif  // FELIS_CHECK_LOWER_H_
