#ifndef FELIS_MIDDLE_LOWER_H_
#define FELIS_MIDDLE_LOWER_H_

#include "check/ctx.h"
#include "middle/mir_builder.h"

namespace felis {

class Lower {
 public:
  Lower(TypeCheckCtx& ctx, std::unique_ptr<mir::File>& file)
      : ctx_(ctx), builder_(file) {}

  void Lowering(std::unique_ptr<ast::File>);

 private:
  TypeCheckCtx& ctx_;
  MIRBuilder builder_;

  std::shared_ptr<mir::Value> LowerStmt(std::unique_ptr<ast::Stmt>);
  void LowerRet(std::unique_ptr<ast::RetStmt>);
  void LowerVarDecl(std::unique_ptr<ast::VarDeclStmt>);
  void LowerAssign(std::unique_ptr<ast::AssignStmt>);

  std::shared_ptr<mir::Value> LowerExpr(std::unique_ptr<ast::Expr>);
  std::shared_ptr<mir::Constant> LowerLit(std::unique_ptr<ast::Lit>);
  std::unique_ptr<mir::Constant> ParseIntLit(std::unique_ptr<ast::Lit>);
  std::unique_ptr<mir::ConstantFloat> ParseFloatLit(std::unique_ptr<ast::Lit>);
  std::shared_ptr<mir::Var> LowerBinary(std::unique_ptr<ast::BinaryExpr>);
  std::shared_ptr<mir::Var> LowerCall(std::unique_ptr<ast::CallExpr>);
  std::shared_ptr<mir::Var> LowerUnary(std::unique_ptr<ast::UnaryExpr>);
  std::shared_ptr<mir::Var> LowerArray(std::unique_ptr<ast::ArrayExpr>);
  std::shared_ptr<mir::Var> LowerIf(std::unique_ptr<ast::If>);
  std::shared_ptr<mir::Value> LowerBlock(std::unique_ptr<ast::Block>);
};

std::unique_ptr<mir::File> Lowering(std::unique_ptr<ast::File> file,
                                    bool is_32bit);
}  // namespace felis

#endif  // FELIS_MIDDLE_LOWER_H_
