#include "mir_builder.h"

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
  mir::FunctionID id = fn_decls.size();
  auto func =
      is_ext
          ? std::make_shared<mir::Func>(id, decl->name, decl->AsFuncType())
          : std::make_shared<mir::Function>(id, decl->name, decl->AsFuncType());
  file->funcs.push_back(func);
  SetFunc(decl, func);
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

std::shared_ptr<mir::LValue> MIRBuilder::CreateAlloc(
    std::shared_ptr<Decl> decl) {
  auto id = current_bb->parent.GenLocalID();
  auto lval = std::make_shared<mir::LValue>(id, decl->type);
  auto inst = std::make_shared<mir::AllocInst>(lval, decl->type);
  current_bb->InsertInst(inst);
  SetVar(decl, lval);
  return lval;
}

std::shared_ptr<mir::LValue> MIRBuilder::CreateAlloc(
    std::shared_ptr<Type> type) {
  auto id = current_bb->parent.GenLocalID();
  auto lval = std::make_shared<mir::LValue>(id, type);
  auto inst = std::make_shared<mir::AllocInst>(lval, type);
  current_bb->InsertInst(inst);
  return lval;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateVal(std::shared_ptr<Type> type) {
  auto id = current_bb->parent.GenLocalID();
  return std::make_shared<mir::Val>(id, type);
}

std::shared_ptr<mir::Val> MIRBuilder::CreateLoad(std::shared_ptr<Decl> decl) {
  auto val = CreateVal(decl->type);
  auto lval = GetVar(decl);
  current_bb->InsertInst(std::make_shared<mir::LoadInst>(val, lval));
  return val;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateLoad(
    std::shared_ptr<mir::LValue> lval) {
  auto val = CreateVal(lval->type);
  current_bb->InsertInst(std::make_shared<mir::LoadInst>(val, lval));
  return val;
}

void MIRBuilder::CreateStore(std::shared_ptr<mir::LValue> lval,
                             std::shared_ptr<mir::RValue> rval) {
  auto inst = std::make_shared<mir::StoreInst>(lval, rval);
  current_bb->InsertInst(inst);
}

std::shared_ptr<mir::Val> MIRBuilder::CreateUnary(
    mir::UnaryInst::Op op, std::shared_ptr<mir::RValue> operand) {
  auto val = CreateVal(operand->type);
  auto binary = std::make_shared<mir::UnaryInst>(val, op, operand);
  current_bb->InsertInst(binary);
  return val;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateBinary(
    mir::BinaryInst::Op op, std::shared_ptr<mir::RValue> lhs,
    std::shared_ptr<mir::RValue> rhs) {
  auto val = CreateVal(lhs->type);
  auto binary = std::make_shared<mir::BinaryInst>(val, op, lhs, rhs);
  current_bb->InsertInst(binary);
  return val;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateCmp(
    mir::CmpInst::Op op, std::shared_ptr<mir::RValue> lhs,
    std::shared_ptr<mir::RValue> rhs) {
  auto val = CreateVal(kTypeBool);
  auto cmp = std::make_shared<mir::CmpInst>(val, op, lhs, rhs);
  current_bb->InsertInst(cmp);
  return val;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateArray(
    std::shared_ptr<Type> type,
    std::vector<std::shared_ptr<mir::RValue>> values) {
  auto val = CreateVal(type);
  auto array = std::make_shared<mir::ArrayInst>(val, values);
  current_bb->InsertInst(array);
  return val;
}

std::shared_ptr<mir::Val> MIRBuilder::CreateCall(
    std::shared_ptr<Decl> decl,
    std::vector<std::shared_ptr<mir::RValue>> args) {
  auto val = CreateVal(decl->AsFuncType()->ret);
  auto func = GetFunction(decl);
  current_bb->InsertInst(std::make_shared<mir::CallInst>(val, args, func));
  return val;
}

std::shared_ptr<mir::BrInst> MIRBuilder::CreateCond(
    std::shared_ptr<mir::RValue> cond, std::shared_ptr<mir::BB> then_bb,
    std::shared_ptr<mir::BB> else_bb) {
  auto inst = std::make_shared<mir::BrInst>(cond, then_bb, else_bb);
  current_bb->InsertInst(inst);
  return inst;
}

void MIRBuilder::CreateGoto(std::shared_ptr<mir::BB> goto_bb) {
  auto inst = std::make_shared<mir::GotoInst>(goto_bb);
  current_bb->InsertInst(inst);
}

void MIRBuilder::CreateRet(std::shared_ptr<mir::RValue> val) {
  auto inst = std::make_shared<mir::RetInst>(val);
  current_bb->InsertInst(inst);
}

}  // namespace felis
