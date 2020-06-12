#ifndef FELIS_MIDDLE_LOWER_H_
#define FELIS_MIDDLE_LOWER_H_

#include "check/ctx.h"
#include "middle/mir_builder.h"

namespace felis {

class Lower {
 public:
  Lower(TypeCheckCtx &ctx, std::unique_ptr<mir::File> &file)
      : ctx_(ctx), builder_(file) {}

  void Lowering(std::unique_ptr<ast::File>);

 private:
  TypeCheckCtx &ctx_;
  MIRBuilder builder_;

  std::shared_ptr<mir::Value> LowerStmt(ast::AstNode *);
  void LowerRet(ast::RetStmt *);
  void LowerVarDecl(ast::VarDeclStmt *);
  void LowerAssign(ast::AssignStmt *);

  std::shared_ptr<mir::Value> LowerExpr(ast::AstNode *);
  std::shared_ptr<mir::Value> LowerLit(ast::Literal *);
  std::shared_ptr<mir::Value> ParseIntLit(ast::Literal *);
  std::shared_ptr<mir::Value> ParseFloatLit(ast::Literal *);
  std::shared_ptr<mir::Value> LowerBinary(ast::Binary *);
  std::shared_ptr<mir::Value> LowerCall(ast::Call *);
  std::shared_ptr<mir::Value> LowerUnary(ast::Unary *);
  std::shared_ptr<mir::Value> LowerArray(ast::Array *);
  std::shared_ptr<mir::Value> LowerIf(ast::If *);
  std::shared_ptr<mir::Value> LowerBlock(ast::Block *);
};

std::unique_ptr<mir::File> Lowering(std::unique_ptr<ast::File>, bool is_32bit);
}  // namespace felis

#endif  // FELIS_MIDDLE_LOWER_H_
