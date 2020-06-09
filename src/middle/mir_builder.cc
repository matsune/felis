#include "mir_builder.h"

#include "macro.h"
#include "string/string.h"

namespace felis {

std::shared_ptr<mir::BB> MIRBuilder::GetBeforeBB(std::shared_ptr<mir::BB> bb) {
  auto fn = bb->parent;
  auto current_bb = fn.entry_bb;
  while (current_bb) {
    if (current_bb->next_bb == bb) return current_bb;
    current_bb = current_bb->next_bb;
  }
  return nullptr;
}

std::shared_ptr<mir::Func> MIRBuilder::CreateFunc(std::shared_ptr<Decl> decl) {
  bool is_ext = decl->kind == DeclKind::EXT;
  auto func =
      is_ext ? std::make_shared<mir::Func>(decl->name, decl->AsFuncTy())
             : std::make_shared<mir::Function>(decl->name, decl->AsFuncTy());
  file->funcs.push_back(func);
  SetDeclFunc(decl, func);
  return func;
}

std::shared_ptr<mir::BB> MIRBuilder::CreateBB(std::shared_ptr<mir::BB> after) {
  auto bb = current_bb;
  if (after) bb = after;
  assert(bb);

  auto insert_bb = std::make_shared<mir::BB>(bb->parent.GenBBID(), bb->parent);
  insert_bb->next_bb = bb->next_bb;
  bb->next_bb = insert_bb;
  return insert_bb;
}

std::shared_ptr<mir::ConstInt> CreateConstInt(std::shared_ptr<Ty> ty,
                                              int64_t val) {
  return std::make_shared<mir::ConstInt>(ty, val);
}
std::shared_ptr<mir::ConstFloat> CreateConstFloat(std::shared_ptr<Ty> ty,
                                                  double val) {
  return std::make_shared<mir::ConstFloat>(ty, val);
}
std::shared_ptr<mir::ConstBool> CreateConstBool(bool val) {
  return std::make_shared<mir::ConstBool>(val);
}

std::shared_ptr<mir::ConstString> MIRBuilder::CreateConstString(
    std::string val) {
  return std::make_shared<mir::ConstString>(val);
}

std::shared_ptr<mir::LValue> MIRBuilder::CreateAlloc(std::shared_ptr<Ty> type) {
  auto lval =
      std::make_shared<mir::LValue>(ToPtr(type), current_bb->parent.GenVarID());
  current_bb->parent.value_list.push_back(lval);
  return lval;
}

void MIRBuilder::CreateAssign(std::shared_ptr<mir::Value> into,
                              std::shared_ptr<mir::Value> value) {
  Insert(std::make_shared<mir::AssignInst>(into, value));
}

std::shared_ptr<mir::RValue> MIRBuilder::CreateRValue(std::shared_ptr<Ty> ty) {
  return std::make_shared<mir::RValue>(ty, current_bb->parent.GenVarID());
}

std::shared_ptr<mir::ConstInt> MIRBuilder::CreateConstInt(
    std::shared_ptr<Ty> ty, int64_t val) {
  return std::make_shared<mir::ConstInt>(ty, val);
}

std::shared_ptr<mir::ConstFloat> MIRBuilder::CreateConstFloat(
    std::shared_ptr<Ty> ty, double val) {
  return std::make_shared<mir::ConstFloat>(ty, val);
}

std::shared_ptr<mir::ConstBool> MIRBuilder::CreateConstBool(bool val) {
  return std::make_shared<mir::ConstBool>(val);
}

std::shared_ptr<mir::RValue> MIRBuilder::CreateUnary(
    std::shared_ptr<Ty> ty, mir::UnaryInst::Op op,
    std::shared_ptr<mir::Value> operand) {
  auto val = CreateRValue(ty);
  Insert(std::make_shared<mir::UnaryInst>(val, op, operand));
  return val;
}

std::shared_ptr<mir::RValue> MIRBuilder::CreateBinary(
    std::shared_ptr<Ty> ty, mir::BinaryInst::Op op,
    std::shared_ptr<mir::Value> lhs, std::shared_ptr<mir::Value> rhs) {
  auto val = CreateRValue(ty);
  Insert(std::make_shared<mir::BinaryInst>(val, op, lhs, rhs));
  return val;
}

std::shared_ptr<mir::RValue> MIRBuilder::CreateCmp(
    mir::CmpInst::Op op, std::shared_ptr<mir::Value> lhs,
    std::shared_ptr<mir::Value> rhs) {
  auto val = CreateRValue(kTypeBool);
  Insert(std::make_shared<mir::CmpInst>(val, op, lhs, rhs));
  return val;
}

std::shared_ptr<mir::RValue> MIRBuilder::CreateCall(
    std::shared_ptr<Decl> decl, std::vector<std::shared_ptr<mir::Value>> args) {
  auto val = CreateRValue(decl->AsFuncTy()->ret);
  Insert(std::make_shared<mir::CallInst>(val, args, GetDeclFunc(decl)));
  return val;
}

std::shared_ptr<mir::BrInst> MIRBuilder::CreateCond(
    std::shared_ptr<mir::Value> cond, std::shared_ptr<mir::BB> then_bb,
    std::shared_ptr<mir::BB> else_bb) {
  auto inst = std::make_shared<mir::BrInst>(cond, then_bb, else_bb);
  Insert(inst);
  return inst;
}

void MIRBuilder::CreateGoto(std::shared_ptr<mir::BB> goto_bb) {
  Insert(std::make_shared<mir::GotoInst>(goto_bb));
}

void MIRBuilder::CreateRet(std::shared_ptr<mir::Value> val) {
  Insert(std::make_shared<mir::RetInst>(val));
}

void MIRBuilder::Insert(std::shared_ptr<mir::Inst> inst) {
  current_bb->InsertInst(inst);
}

}  // namespace felis
