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

std::shared_ptr<mir::Var> MIRBuilder::CreateVar(std::shared_ptr<Ty> type,
                                                bool alloc, std::string name) {
  auto var = std::make_shared<mir::Var>(current_bb->parent.GenVarID(), type,
                                        alloc, name);
  current_bb->parent.var_list.push_back(var);
  return var;
}

std::shared_ptr<mir::Var> MIRBuilder::CreateAlloc(std::shared_ptr<Ty> type,
                                                  std::string name) {
  return CreateVar(type, true, name);
}

void MIRBuilder::CreateAssign(std::shared_ptr<mir::Var> into,
                              std::shared_ptr<mir::Value> value) {
  Insert(std::make_shared<mir::AssignInst>(into, value));
}

std::shared_ptr<mir::Var> MIRBuilder::CreateUnary(
    mir::UnaryInst::Op op, std::shared_ptr<mir::Value> operand) {
  auto var = CreateVar(operand->type);
  Insert(std::make_shared<mir::UnaryInst>(var, op, operand));
  return var;
}

std::shared_ptr<mir::Var> MIRBuilder::CreateBinary(
    mir::BinaryInst::Op op, std::shared_ptr<mir::Value> lhs,
    std::shared_ptr<mir::Value> rhs) {
  auto var = CreateVar(lhs->type);
  Insert(std::make_shared<mir::BinaryInst>(var, op, lhs, rhs));
  return var;
}

std::shared_ptr<mir::Var> MIRBuilder::CreateCmp(
    mir::CmpInst::Op op, std::shared_ptr<mir::Value> lhs,
    std::shared_ptr<mir::Value> rhs) {
  auto var = CreateVar(kTypeBool);
  Insert(std::make_shared<mir::CmpInst>(var, op, lhs, rhs));
  return var;
}

std::shared_ptr<mir::Var> MIRBuilder::CreateCall(
    std::shared_ptr<Decl> decl, std::vector<std::shared_ptr<mir::Value>> args) {
  auto var = CreateVar(decl->AsFuncTy()->ret);
  Insert(std::make_shared<mir::CallInst>(var, args, GetDeclFunc(decl)));
  return var;
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
