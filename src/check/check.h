#ifndef FELIS_CHECK_CHECK_H_
#define FELIS_CHECK_CHECK_H_

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "check/decl.h"
#include "check/hir.h"
#include "check/scope.h"
#include "check/type.h"
#include "string/string.h"
#include "syntax/ast.h"

namespace felis {

class Checker {
 public:
  Checker() : currentScope_(std::make_shared<Scope>(nullptr)){};
  void SetupBuiltin();
  void Check(std::unique_ptr<ast::File>&);

 private:
  std::shared_ptr<Scope> currentScope_;
  std::map<ast::Node*, std::shared_ptr<Decl>> node_decl_;
  std::shared_ptr<Decl> currentFunc_;

  void CheckFnDecl(std::unique_ptr<ast::FnDecl>&);

  void CheckStmt(std::unique_ptr<ast::Stmt>&);
  void CheckRetStmt(ast::RetStmt*);
  void CheckVarDeclStmt(ast::VarDeclStmt*);
  void CheckAssignStmt(ast::AssignStmt*);
  void CheckIfStmt(ast::IfStmt*);
  void CheckBlock(ast::Block*);

  std::unique_ptr<hir::Expr> MakeExpr(ast::Expr* expr);
  std::unique_ptr<hir::Constant> MakeLit(ast::Lit* lit);

  std::unique_ptr<hir::Constant> MakeConstBinary(hir::Constant*, hir::Constant*,
                                                 ast::BinOp);
  void MakeConstUnary(hir::Constant*, ast::UnOp);

  std::unique_ptr<hir::IntConstant> ParseInt(ast::Lit*);
  double ParseFloat(ast::Lit*);
  void TryExpTy(hir::Expr*, std::shared_ptr<Type>);
  void TryConstantTy(hir::Constant* cons, std::shared_ptr<Type> ty);
  void CheckBinary(std::unique_ptr<hir::Expr>& lhs,
                   std::unique_ptr<hir::Expr>& rhs, ast::BinOp op);

  void RecordNodeDecl(ast::Node*, std::shared_ptr<Decl>);
  std::shared_ptr<Decl> InsertFnDecl(
      bool isExt, const std::unique_ptr<ast::FnProto>& proto);
  void OpenScope();
  void CloseScope();
  bool CanDecl(std::string name);
  std::shared_ptr<Decl> LookupDecl(std::string name);
  std::shared_ptr<Type> LookupType(std::string name);

  void DebugScope();
};

}  // namespace felis

#endif  // FELIS_CHECK_CHECK_H_
