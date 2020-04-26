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
  void Check(std::unique_ptr<File>&);

 private:
  std::shared_ptr<Scope> currentScope_;
  std::map<Node*, std::shared_ptr<Decl>> node_decl_;

  void CheckFnDecl(std::unique_ptr<FnDecl>&);
  void CheckStmt(std::unique_ptr<Stmt>&);
  std::unique_ptr<hir::Constant> MakeLit(Lit* lit);
  std::unique_ptr<hir::Expr> MakeExp(Expr* expr);
  void TryExpTy(hir::Expr*, std::shared_ptr<Type>);
  void TryConstantTy(hir::Constant* cons, std::shared_ptr<Type> ty);
  void CheckBinary(std::unique_ptr<hir::Expr>& lhs,
                   std::unique_ptr<hir::Expr>& rhs, BinOp op);
  void RecordNodeDecl(Node*, std::shared_ptr<Decl>);
  std::shared_ptr<Decl> InsertFnDecl(bool isExt,
                                     const std::unique_ptr<FnProto>& proto);
  void OpenScope();
  void CloseScope();
  bool CanDecl(std::string name);
  std::shared_ptr<Decl> LookupDecl(std::string name);
  std::shared_ptr<Type> LookupType(std::string name);
};

}  // namespace felis

#endif  // FELIS_CHECK_CHECK_H_
