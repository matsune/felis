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

  std::shared_ptr<mir::RValue> LowerStmt(std::unique_ptr<ast::Stmt>,
                                         std::shared_ptr<mir::BB> = nullptr);

  std::shared_ptr<mir::RValue> LowerExpr(std::unique_ptr<ast::Expr>,
                                         std::shared_ptr<mir::BB> = nullptr);

  std::unique_ptr<mir::Constant> LowerLit(std::unique_ptr<ast::Lit>);
  std::unique_ptr<mir::Constant> ParseIntLit(std::unique_ptr<ast::Lit>);
  std::unique_ptr<mir::ConstantFloat> ParseFloatLit(std::unique_ptr<ast::Lit>);

  std::shared_ptr<mir::RValue> LowerIf(std::unique_ptr<ast::If>,
                                       std::shared_ptr<mir::BB>);
  std::shared_ptr<mir::RValue> LowerBlock(std::unique_ptr<ast::Block>,
                                          std::shared_ptr<mir::BB>);
};

std::unique_ptr<mir::File> Lowering(std::unique_ptr<ast::File> file,
                                    bool is_32bit);
}  // namespace felis

#endif  // FELIS_MIDDLE_LOWER_H_
