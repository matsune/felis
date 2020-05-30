#ifndef FELIS_CHECK_LOW_H_
#define FELIS_CHECK_LOW_H_

#include <map>
#include <memory>

#include "check/type_checker.h"
#include "node/ast.h"
#include "node/mir.h"

namespace felis {

class MIRBuilder {
 public:
  MIRBuilder() : file(std::make_unique<mir::File>()) {}

  std::shared_ptr<mir::Func> CreateFunc(std::shared_ptr<Decl>);
  std::shared_ptr<mir::BB> CreateBlock();
  std::shared_ptr<mir::Val> CreateLoad(std::shared_ptr<Decl>);
  std::shared_ptr<mir::Alloc> CreateAlloc(std::shared_ptr<Decl>);
  std::shared_ptr<mir::Val> CreateVal(std::shared_ptr<Type>);
  std::shared_ptr<mir::Val> CreateBinary(std::shared_ptr<Type>,
                                         std::shared_ptr<mir::Value>,
                                         std::shared_ptr<mir::Value>,
                                         mir::Binary::Op);
  std::shared_ptr<mir::Val> CreateComp(std::shared_ptr<Type>,
                                       std::shared_ptr<mir::Value>,
                                       std::shared_ptr<mir::Value>,
                                       mir::Comp::Op);
  std::shared_ptr<mir::Val> CreateUnary(std::shared_ptr<Type>,
                                        std::shared_ptr<mir::Value>,
                                        mir::Unary::Op);
  std::shared_ptr<mir::Val> CreateArray(
      std::shared_ptr<Type>, std::vector<std::shared_ptr<mir::Value>>);
  std::shared_ptr<mir::Val> CreateCall(
      std::shared_ptr<Decl>, std::vector<std::shared_ptr<mir::Value>>);
  void CreateStore(std::shared_ptr<mir::Alloc>, std::shared_ptr<mir::Value>);
  void CreateRet(std::shared_ptr<mir::Value>);
  std::shared_ptr<mir::Cond> CreateCond(std::shared_ptr<mir::Value>);

  std::unique_ptr<mir::File> file;

  std::shared_ptr<mir::Func> GetFunction(std::shared_ptr<Decl> decl) {
    return fn_decls.at(decl);
  }

  std::shared_ptr<mir::Alloc> GetAlloc(std::shared_ptr<Decl> decl) {
    return alloc_map.at(decl);
  }

  void SetInsertPoint(std::shared_ptr<mir::BB> bb) { current_bb = bb; }

 private:
  std::map<std::shared_ptr<Decl>, std::shared_ptr<mir::Func>> fn_decls;
  std::map<std::shared_ptr<Decl>, std::shared_ptr<mir::Alloc>> alloc_map;
  std::shared_ptr<mir::BB> current_bb;
};

class Lower {
 public:
  Lower(IdentDeclMap& ident_decl_map, ExprTypeMap& expr_type_map)
      : ident_decl_map_(ident_decl_map), expr_type_map_(expr_type_map) {}

  std::unique_ptr<mir::File> Lowering(std::unique_ptr<ast::File>);

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

  std::shared_ptr<mir::Value> LowerStmt(std::unique_ptr<ast::Stmt>);
  std::shared_ptr<mir::Value> LowerExpr(std::unique_ptr<ast::Expr>);
  std::unique_ptr<mir::Value> LowerLit(std::unique_ptr<ast::Lit>);
  std::unique_ptr<mir::Constant> ParseIntLit(std::unique_ptr<ast::Lit>);
  std::unique_ptr<mir::ConstantFloat> ParseFloatLit(std::unique_ptr<ast::Lit>);
  std::shared_ptr<mir::Value> LowerBlock(std::unique_ptr<ast::Block>);
  std::shared_ptr<mir::Value> LowerIf(std::unique_ptr<ast::If>);
};

std::unique_ptr<mir::File> Lowering(std::unique_ptr<ast::File> file,
                                    bool is_32bit);
}  // namespace felis

#endif  // FELIS_CHECK_LOW_H_
