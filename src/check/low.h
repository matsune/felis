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
  MIRBuilder(std::unique_ptr<mir::File>& file) : file(file) {}

  inline std::shared_ptr<mir::Func> GetFunction(std::shared_ptr<Decl> decl) {
    return fn_decls.at(decl);
  }

  inline std::shared_ptr<mir::LValue> GetVar(std::shared_ptr<Decl> decl) {
    return var_map.at(decl);
  }

  void SetInsertBB(std::shared_ptr<mir::BB> bb) { current_bb = bb; }

  const std::shared_ptr<mir::BB>& GetInsertBB() const { return current_bb; }

  std::shared_ptr<mir::BB> GetBeforeBB(std::shared_ptr<mir::BB>);

  std::shared_ptr<mir::Func> CreateFunc(std::shared_ptr<Decl>);

  std::shared_ptr<mir::BB> CreateBB(std::shared_ptr<mir::BB> after = nullptr);

  std::shared_ptr<mir::LValue> CreateAlloc(std::shared_ptr<Decl>);
  std::shared_ptr<mir::LValue> CreateAlloc(std::shared_ptr<Type>);

  std::shared_ptr<mir::Val> CreateVal(std::shared_ptr<Type>);

  std::shared_ptr<mir::Val> CreateLoad(std::shared_ptr<Decl>);
  std::shared_ptr<mir::Val> CreateLoad(std::shared_ptr<mir::LValue>);

  void CreateStore(std::shared_ptr<mir::LValue>, std::shared_ptr<mir::RValue>);

  std::shared_ptr<mir::Val> CreateUnary(mir::UnaryInst::Op,
                                        std::shared_ptr<mir::RValue>);

  std::shared_ptr<mir::Val> CreateBinary(mir::BinaryInst::Op,
                                         std::shared_ptr<mir::RValue>,
                                         std::shared_ptr<mir::RValue>);

  std::shared_ptr<mir::Val> CreateCmp(mir::CmpInst::Op,
                                      std::shared_ptr<mir::RValue>,
                                      std::shared_ptr<mir::RValue>);

  std::shared_ptr<mir::Val> CreateArray(
      std::shared_ptr<Type>, std::vector<std::shared_ptr<mir::RValue>>);

  std::shared_ptr<mir::Val> CreateCall(
      std::shared_ptr<Decl>, std::vector<std::shared_ptr<mir::RValue>>);

  std::shared_ptr<mir::BrInst> CreateCond(std::shared_ptr<mir::RValue>);

  void CreateGoto(std::shared_ptr<mir::BB>);

  void CreateRet(std::shared_ptr<mir::RValue>);

 private:
  std::unique_ptr<mir::File>& file;
  std::map<std::shared_ptr<Decl>, std::shared_ptr<mir::Func>> fn_decls;
  std::map<std::shared_ptr<Decl>, std::shared_ptr<mir::LValue>> var_map;
  std::shared_ptr<mir::BB> current_bb;

  inline void SetFunc(std::shared_ptr<Decl> decl,
                      std::shared_ptr<mir::Func> func) {
    fn_decls[decl] = func;
  }

  inline void SetVar(std::shared_ptr<Decl> decl,
                     std::shared_ptr<mir::LValue> lval) {
    var_map[decl] = lval;
  }
};

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

#endif  // FELIS_CHECK_LOW_H_
