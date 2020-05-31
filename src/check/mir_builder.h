#ifndef FELIS_CHECK_MIR_BUILDER_H_
#define FELIS_CHECK_MIR_BUILDER_H_

#include <memory>

#include "check/decl.h"
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

  void CreateRet(std::shared_ptr<mir::RValue> = nullptr);

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

}  // namespace felis

#endif  // FELIS_CHECK_MIR_BUILDER_H_
