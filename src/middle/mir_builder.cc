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
  mir::FunctionID id = fn_decls.size();
  auto func =
      is_ext
          ? std::make_shared<mir::Func>(id, decl->name, decl->AsFuncTy())
          : std::make_shared<mir::Function>(id, decl->name, decl->AsFuncTy());
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

std::shared_ptr<mir::Var> MIRBuilder::CreateVar(std::shared_ptr<Ty> type,
                                                bool alloc, std::string name) {
  auto id = current_bb->parent.GenVarID();
  auto var = std::make_shared<mir::Var>(id, type, alloc, name);
  current_bb->parent.var_list.push_back(var);
  return var;
}

std::shared_ptr<mir::Var> MIRBuilder::CreateAlloc(std::shared_ptr<Decl> decl) {
  auto var = CreateVar(decl->type, true, decl->name);
  SetVar(decl, var);
  return var;
}

void MIRBuilder::CreateAssign(std::shared_ptr<mir::Value> into,
                              std::shared_ptr<mir::Value> value) {
  auto inst = std::make_shared<mir::AssignInst>(into, value);
  current_bb->InsertInst(inst);
}

std::shared_ptr<mir::Var> MIRBuilder::CreateUnary(
    mir::UnaryInst::Op op, std::shared_ptr<mir::Value> operand) {
  auto var = CreateVar(operand->type, false);
  auto binary = std::make_shared<mir::UnaryInst>(var, op, operand);
  current_bb->InsertInst(binary);
  return var;
}

std::shared_ptr<mir::Var> MIRBuilder::CreateBinary(
    mir::BinaryInst::Op op, std::shared_ptr<mir::Value> lhs,
    std::shared_ptr<mir::Value> rhs) {
  auto var = CreateVar(lhs->type, false);
  auto binary = std::make_shared<mir::BinaryInst>(var, op, lhs, rhs);
  current_bb->InsertInst(binary);
  return var;
}

std::shared_ptr<mir::Var> MIRBuilder::CreateCmp(
    mir::CmpInst::Op op, std::shared_ptr<mir::Value> lhs,
    std::shared_ptr<mir::Value> rhs) {
  auto var = CreateVar(kTypeBool, false);
  auto cmp = std::make_shared<mir::CmpInst>(var, op, lhs, rhs);
  current_bb->InsertInst(cmp);
  return var;
}

std::shared_ptr<mir::Var> MIRBuilder::CreateCall(
    std::shared_ptr<Decl> decl, std::vector<std::shared_ptr<mir::Value>> args) {
  auto var = CreateVar(decl->AsFuncTy()->ret, false);
  auto func = GetFunction(decl);
  current_bb->InsertInst(std::make_shared<mir::CallInst>(var, args, func));
  return var;
}

std::shared_ptr<mir::BrInst> MIRBuilder::CreateCond(
    std::shared_ptr<mir::Value> cond, std::shared_ptr<mir::BB> then_bb,
    std::shared_ptr<mir::BB> else_bb) {
  auto inst = std::make_shared<mir::BrInst>(cond, then_bb, else_bb);
  current_bb->InsertInst(inst);
  return inst;
}

void MIRBuilder::CreateGoto(std::shared_ptr<mir::BB> goto_bb) {
  auto inst = std::make_shared<mir::GotoInst>(goto_bb);
  current_bb->InsertInst(inst);
}

void MIRBuilder::CreateRet(std::shared_ptr<mir::Value> val) {
  auto inst = std::make_shared<mir::RetInst>(val);
  current_bb->InsertInst(inst);
}

}  // namespace felis
