#ifndef FELIS_CHECK_LOWER_H_
#define FELIS_CHECK_LOWER_H_

#include <map>
#include <memory>

#include "node/ast.h"
#include "node/hir.h"

namespace felis {

// class Lower {
// public:
//  Lower(std::map<ast::AstNode *, std::shared_ptr<Decl>> &ast_decl)
//      : ast_decl_(ast_decl){};
//  std::unique_ptr<hir::File> Lowering(std::unique_ptr<ast::File>);
//
// private:
//  FuncType *current_func_;
//  std::map<ast::AstNode *, std::shared_ptr<Decl>> &ast_decl_;
//
//  std::unique_ptr<hir::Stmt> LowerStmt(std::unique_ptr<ast::Stmt>);
//  std::unique_ptr<hir::RetStmt> LowerRet(std::unique_ptr<ast::RetStmt>);
//  std::unique_ptr<hir::VarDeclStmt> LowerVarDecl(
//      std::unique_ptr<ast::VarDeclStmt>);
//  std::unique_ptr<hir::AssignStmt> LowerAssign(
//      std::unique_ptr<ast::AssignStmt>);
//
//  std::unique_ptr<hir::Expr> LowerExpr(std::unique_ptr<ast::Expr>);
//  std::unique_ptr<hir::If> LowerIf(std::unique_ptr<ast::If>);
//  std::unique_ptr<hir::Block> LowerBlock(std::unique_ptr<ast::Block>);
//
//  std::unique_ptr<hir::Constant> LowerLit(std::unique_ptr<ast::Lit>);
//  std::unique_ptr<hir::IntConstant> ParseInt(std::unique_ptr<ast::Lit>);
//  double ParseFloat(std::unique_ptr<ast::Lit>);
//
//  std::unique_ptr<hir::Constant> ConstBinary(std::unique_ptr<hir::Binary>);
//  std::unique_ptr<hir::Expr> CheckBinary(std::unique_ptr<hir::Binary>);
//  std::unique_ptr<hir::Constant> ConstUnary(std::unique_ptr<hir::Constant>,
//                                            hir::Unary::Op);
//
//  std::unique_ptr<hir::Expr> MatchExprTy(std::unique_ptr<hir::Expr>,
//                                         std::shared_ptr<Type>);
//  std::unique_ptr<hir::Expr> MatchJustType(std::unique_ptr<hir::Expr>,
//                                           std::shared_ptr<Type>);
//  std::unique_ptr<hir::Expr> MatchIfType(std::unique_ptr<hir::If>,
//                                         std::shared_ptr<Type>);
//  std::unique_ptr<hir::Expr> MatchBlockType(std::unique_ptr<hir::Block>,
//                                            std::shared_ptr<Type>);
//  std::unique_ptr<hir::Expr> MatchValueType(std::unique_ptr<hir::Value>,
//                                            std::shared_ptr<Type>);
//  std::unique_ptr<hir::Expr> MatchConstantType(std::unique_ptr<hir::Constant>,
//                                               std::shared_ptr<Type>);
//
//  void CheckNotVoidType(const hir::Expr *expr);
//};

}  // namespace felis

#endif  // FELIS_CHECK_LOWER_H_
