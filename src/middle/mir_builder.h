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

  inline std::shared_ptr<mir::Value> GetDeclValue(std::shared_ptr<Decl> decl) {
    return current_bb->parent.decl_value_map.at(decl);
  }
  inline void SetDeclValue(std::shared_ptr<Decl> decl,
                           std::shared_ptr<mir::Value> value) {
    current_bb->parent.decl_value_map[decl] = value;
  }

  void SetInsertBB(std::shared_ptr<mir::BB> bb) { current_bb = bb; }
  const std::shared_ptr<mir::BB> &GetInsertBB() const { return current_bb; }

  std::shared_ptr<mir::BB> GetBeforeBB(std::shared_ptr<mir::BB>);

  std::shared_ptr<mir::Func> CreateFunc(std::shared_ptr<Decl>);

  std::shared_ptr<mir::BB> CreateBB(std::shared_ptr<mir::BB> after = nullptr);

  std::shared_ptr<mir::ConstInt> CreateConstInt(std::shared_ptr<Ty>, int64_t);
  std::shared_ptr<mir::ConstFloat> CreateConstFloat(std::shared_ptr<Ty>,
                                                    double);
  std::shared_ptr<mir::ConstBool> CreateConstBool(bool);
  std::shared_ptr<mir::ConstString> CreateConstString(std::string);
  std::shared_ptr<mir::Result> CreateAlloc(std::shared_ptr<Ty>);

  //  std::shared_ptr<mir::Value> CreateValue(std::shared_ptr<Ty>,bool);
  //  std::shared_ptr<mir::RValue> CreateAllocatedRValue(std::shared_ptr<Ty>);
  std::shared_ptr<mir::Result> CreateResult(std::shared_ptr<Ty>);
  void CreateAssign(std::shared_ptr<mir::Value>, std::shared_ptr<mir::Value>);

  std::shared_ptr<mir::Result> CreateUnary(std::shared_ptr<Ty>,
                                           mir::UnaryInst::Op,
                                           std::shared_ptr<mir::Value>);
  std::shared_ptr<mir::Result> CreateBinary(std::shared_ptr<Ty>,
                                            mir::BinaryInst::Op,
                                            std::shared_ptr<mir::Value>,
                                            std::shared_ptr<mir::Value>);
  std::shared_ptr<mir::Result> CreateCmp(mir::CmpInst::Op,
                                         std::shared_ptr<mir::Value>,
                                         std::shared_ptr<mir::Value>);
  std::shared_ptr<mir::Result> CreateCall(
      std::shared_ptr<Decl>, std::vector<std::shared_ptr<mir::Value>>);
  //
  //  std::shared_ptr<mir::Var> CreateArray(
  //      std::shared_ptr<ArrayTy>, std::vector<std::shared_ptr<mir::Value>>);
  //

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
