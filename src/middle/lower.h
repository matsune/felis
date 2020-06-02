#ifndef FELIS_MIDDLE_LOWER_H_
#define FELIS_MIDDLE_LOWER_H_

#include "check/ctx.h"
#include "check/stmt_result.h"
#include "middle/mir_builder.h"

namespace felis {

using LowStmtResult = StmtResult<mir::RValue>;

class Lower {
 public:
  Lower(TypeCheckCtx& ctx, std::unique_ptr<mir::File>& file)
      : ctx_(ctx), builder_(file) {}

  void Lowering(std::unique_ptr<ast::File>);

 private:
  TypeCheckCtx& ctx_;
  MIRBuilder builder_;

  LowStmtResult LowerStmt(std::unique_ptr<ast::Stmt>);
  LowStmtResult LowerRet(std::unique_ptr<ast::RetStmt>);
  LowStmtResult LowerVarDecl(std::unique_ptr<ast::VarDeclStmt>);
  LowStmtResult LowerAssign(std::unique_ptr<ast::AssignStmt>);

  LowStmtResult LowerExpr(std::unique_ptr<ast::Expr>);
  LowStmtResult LowerIdent(std::unique_ptr<ast::Ident>);
  LowStmtResult LowerLit(std::unique_ptr<ast::Lit>);
  std::unique_ptr<mir::Constant> ParseIntLit(std::unique_ptr<ast::Lit>);
  std::unique_ptr<mir::ConstantFloat> ParseFloatLit(std::unique_ptr<ast::Lit>);
  LowStmtResult LowerBinary(std::unique_ptr<ast::BinaryExpr>);
  LowStmtResult LowerCall(std::unique_ptr<ast::CallExpr>);
  LowStmtResult LowerUnary(std::unique_ptr<ast::UnaryExpr>);
  LowStmtResult LowerArray(std::unique_ptr<ast::ArrayExpr>);
  LowStmtResult LowerIf(std::unique_ptr<ast::If>);
  LowStmtResult LowerBlock(std::unique_ptr<ast::Block>);
};

std::unique_ptr<mir::File> Lowering(std::unique_ptr<ast::File> file,
                                    bool is_32bit);
}  // namespace felis

#endif  // FELIS_MIDDLE_LOWER_H_
