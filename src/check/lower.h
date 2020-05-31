#ifndef FELIS_CHECK_LOWER_H_
#define FELIS_CHECK_LOWER_H_

#include "check/mir_builder.h"
#include "check/type_checker.h"

namespace felis {

class Lower {
 public:
  Lower(IdentDeclMap& ident_decl_map, ExprTypeMap& expr_type_map,
        std::unique_ptr<mir::File>& file)
      : ident_decl_map_(ident_decl_map),
        expr_type_map_(expr_type_map),
        builder_(file) {}

  void Lowering(std::unique_ptr<ast::File>);

 private:
  const IdentDeclMap& ident_decl_map_;
  const ExprTypeMap& expr_type_map_;
  MIRBuilder builder_;

  auto& GetDecl(const std::unique_ptr<ast::Ident>& t) const {
    std::cout << "GetDecl " << t.get() << std::endl;
    return ident_decl_map_.at(t.get());
  }

  template <typename K>
  std::shared_ptr<FixedType> GetType(const std::unique_ptr<K>& n) const {
    return std::dynamic_pointer_cast<FixedType>(expr_type_map_.at(n.get()));
  }

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

#endif  // FELIS_CHECK_LOWER_H_
