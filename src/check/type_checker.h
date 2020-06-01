#ifndef FELIS_CHECK_TYPE_CHECKER_H_
#define FELIS_CHECK_TYPE_CHECKER_H_

#include <map>
#include <memory>

#include "check/ctx.h"
#include "check/decl_checker.h"
#include "node/ast.h"

namespace felis {

struct StmtType {
  enum Kind { NON, RET, TYPE };
  std::shared_ptr<Type> ty;
  StmtType::Kind kind;

  StmtType(StmtType::Kind kind, std::shared_ptr<Type> ty = nullptr)
      : kind(kind), ty(ty) {
    if (kind == Kind::NON) assert(ty == nullptr);
    if (kind == Kind::RET) assert(ty == nullptr);
    if (kind == Kind::TYPE) assert(ty != nullptr);
  }

  bool IsNon() const { return kind == StmtType::Kind::NON; }
  bool IsRet() const { return kind == StmtType::Kind::RET; }
  bool IsType() const { return kind == StmtType::Kind::TYPE; }
};

class TypeChecker {
 public:
  TypeChecker(TypeCheckCtx &ctx) : ctx_(ctx), decl_ck_(ctx.Is32bit()) {}

  void Check(const std::unique_ptr<ast::File> &);

 private:
  TypeCheckCtx &ctx_;
  DeclChecker decl_ck_;
  std::shared_ptr<FuncType> current_func_;

  StmtType CheckStmt(const std::unique_ptr<ast::Stmt> &);
  void CheckVarDecl(const std::unique_ptr<ast::VarDeclStmt> &);
  void CheckAssign(const std::unique_ptr<ast::AssignStmt> &);
  StmtType CheckBlock(const std::unique_ptr<ast::Block> &);
  StmtType CheckExpr(const std::unique_ptr<ast::Expr> &);
  StmtType CheckIf(const std::unique_ptr<ast::If> &);
};

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_CHECKER_H_
