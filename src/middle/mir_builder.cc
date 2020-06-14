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
  auto func = is_ext ? std::make_shared<mir::Func>(decl->name, decl->type)
                     : std::make_shared<mir::Function>(decl->name, decl->type);
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

std::shared_ptr<mir::ConstInt> MIRBuilder::CreateConstInt(
    std::shared_ptr<Type> ty, int64_t val) {
  return std::make_shared<mir::ConstInt>(ty, val);
}
std::shared_ptr<mir::ConstFloat> MIRBuilder::CreateConstFloat(
    std::shared_ptr<Type> ty, double val) {
  return std::make_shared<mir::ConstFloat>(ty, val);
}
std::shared_ptr<mir::ConstBool> MIRBuilder::CreateConstBool(bool val) {
  return std::make_shared<mir::ConstBool>(val);
}
std::shared_ptr<mir::ConstString> MIRBuilder::CreateConstString(
    std::string val) {
  return std::make_shared<mir::ConstString>(val);
}

std::shared_ptr<mir::Result> MIRBuilder::CreateResult(
    std::shared_ptr<Type> ty) {
  return std::make_shared<mir::Result>(ty, current_bb->parent.GenResultID());
}
std::shared_ptr<mir::Result> MIRBuilder::CreateAlloc(std::shared_ptr<Type> ty) {
  auto result = std::make_shared<mir::Result>(Type::MakePtr(ty),
                                              current_bb->parent.GenResultID());
  current_bb->parent.InsertAlloc(result);
  return result;
}

void MIRBuilder::CreateAssign(std::shared_ptr<mir::Value> into,
                              std::shared_ptr<mir::Value> value) {
  Insert(std::make_shared<mir::AssignInst>(into, value));
}

std::shared_ptr<mir::Result> MIRBuilder::CreateUnary(
    std::shared_ptr<Type> ty, mir::UnaryInst::Op op,
    std::shared_ptr<mir::Value> operand) {
  auto result = CreateResult(ty);
  Insert(std::make_shared<mir::UnaryInst>(result, op, operand));
  return result;
}

std::shared_ptr<mir::Result> MIRBuilder::CreateBinary(
    std::shared_ptr<Type> ty, mir::BinaryInst::Op op,
    std::shared_ptr<mir::Value> lhs, std::shared_ptr<mir::Value> rhs) {
  auto result = CreateResult(ty);
  Insert(std::make_shared<mir::BinaryInst>(result, op, lhs, rhs));
  return result;
}

std::shared_ptr<mir::Result> MIRBuilder::CreateCmp(
    mir::CmpInst::Op op, std::shared_ptr<mir::Value> lhs,
    std::shared_ptr<mir::Value> rhs) {
  auto result = CreateResult(Type::MakeBool());
  Insert(std::make_shared<mir::CmpInst>(result, op, lhs, rhs));
  return result;
}
std::shared_ptr<mir::Result> MIRBuilder::CreateCall(
    std::shared_ptr<Decl> decl, std::vector<std::shared_ptr<mir::Value>> args) {
  auto result = CreateResult(decl->type->GetRet());
  Insert(std::make_shared<mir::CallInst>(result, args, GetDeclFunc(decl)));
  return result;
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
