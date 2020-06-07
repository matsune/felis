#ifndef FELIS_MIDDLE_MIR_BUILDER_H_
#define FELIS_MIDDLE_MIR_BUILDER_H_

#include <memory>

#include "check/decl.h"
#include "node/mir.h"

namespace felis {

class MIRBuilder {
 public:
  MIRBuilder(std::unique_ptr<mir::File> &file) : file(file) {}

  template <typename T = mir::Func>
  std::shared_ptr<T> GetDeclFunc(std::shared_ptr<Decl> decl) {
    return std::dynamic_pointer_cast<T>(file->decl_fn_map.at(decl));
  }
  inline void SetDeclFunc(std::shared_ptr<Decl> decl,
                          std::shared_ptr<mir::Func> func) {
    file->decl_fn_map[decl] = func;
  }

  inline std::shared_ptr<mir::Var> GetDeclVar(std::shared_ptr<Decl> decl) {
    return current_bb->parent.decl_var_map.at(decl);
  }
  inline void SetDeclVar(std::shared_ptr<Decl> decl,
                         std::shared_ptr<mir::Var> var) {
    current_bb->parent.decl_var_map[decl] = var;
  }

  void SetInsertBB(std::shared_ptr<mir::BB> bb) { current_bb = bb; }
  const std::shared_ptr<mir::BB> &GetInsertBB() const { return current_bb; }

  std::shared_ptr<mir::BB> GetBeforeBB(std::shared_ptr<mir::BB>);

  std::shared_ptr<mir::Func> CreateFunc(std::shared_ptr<Decl>);

  std::shared_ptr<mir::BB> CreateBB(std::shared_ptr<mir::BB> after = nullptr);

  std::shared_ptr<mir::Var> CreateVar(std::shared_ptr<Ty>, bool alloc = false,
                                      std::string name = "");
  std::shared_ptr<mir::Var> CreateAlloc(std::shared_ptr<Ty>,
                                        std::string name = "");

  void CreateAssign(std::shared_ptr<mir::Var>, std::shared_ptr<mir::Value>);

  std::shared_ptr<mir::Var> CreateUnary(mir::UnaryInst::Op,
                                        std::shared_ptr<mir::Value>);
  std::shared_ptr<mir::Var> CreateBinary(mir::BinaryInst::Op,
                                         std::shared_ptr<mir::Value>,
                                         std::shared_ptr<mir::Value>);
  std::shared_ptr<mir::Var> CreateCmp(mir::CmpInst::Op,
                                      std::shared_ptr<mir::Value>,
                                      std::shared_ptr<mir::Value>);

  std::shared_ptr<mir::Var> CreateArray(
      std::shared_ptr<ArrayTy>, std::vector<std::shared_ptr<mir::Value>>);

  std::shared_ptr<mir::Var> CreateCall(
      std::shared_ptr<Decl>, std::vector<std::shared_ptr<mir::Value>>);

  std::shared_ptr<mir::BrInst> CreateCond(std::shared_ptr<mir::Value>,
                                          std::shared_ptr<mir::BB>,
                                          std::shared_ptr<mir::BB>);

  void CreateGoto(std::shared_ptr<mir::BB>);

  void CreateRet(std::shared_ptr<mir::Value> = nullptr);

  void Insert(std::shared_ptr<mir::Inst>);

  std::shared_ptr<mir::BB> current_bb;

 private:
  std::unique_ptr<mir::File> &file;
};

}  // namespace felis

#endif  // FELIS_MIDDLE_MIR_BUILDER_H_
