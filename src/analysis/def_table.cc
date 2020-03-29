#include "def_table.h"

#include <algorithm>

#include "string/string.h"

namespace felis {

DefId DefTable::InsertFn(std::unique_ptr<DefFn> fn) {
  Stack *stack;
  if (fn->depth == 0) {
    stack = &global_;
  } else {
    stack = &local_;
  }
  auto id = nextId_++;
  fn->id = id;
  stack->Insert(std::move(fn));
  return id;
}

DefId DefTable::InsertVar(std::unique_ptr<DefVar> var) {
  Stack *stack;
  if (var->depth == 0) {
    stack = &global_;
  } else {
    stack = &local_;
  }
  auto id = nextId_++;
  var->id = id;
  stack->Insert(std::move(var));
  return id;
}

bool DefTable::IsDeclaredFn(uint8_t depth, std::string name) {
  auto def = FindFn(depth, name);
  return def != nullptr;
}

bool DefTable::IsDeclaredVar(uint8_t depth, std::string name) {
  auto def = FindVar(depth, name);
  return def != nullptr && def->depth == depth;
}

DefFn *DefTable::FindFn(uint8_t depth, std::string name) {
  Stack *stack;
  if (depth == 0) {
    stack = &global_;
  } else {
    stack = &local_;
  }
  return stack->FindFn(depth, name);
}

DefVar *DefTable::FindVar(uint8_t depth, std::string name) {
  Stack *stack;
  if (depth == 0) {
    stack = &global_;
  } else {
    stack = &local_;
  }
  return stack->FindVar(depth, name);
}

void printDef(const std::unique_ptr<Def> &def) {
  if (def->isFn()) {
    // fn
    auto fn = reinterpret_cast<DefFn *>(def.get());
    std::cout << "KIND: " << (fn->isExt ? "ext" : "fn") << std::endl;
    for (int i = 0; i < fn->args.size(); i++) {
      std::cout << "ARG " << i << ": " << ToString(fn->args.at(i)) << std::endl;
    }
    std::cout << "RET TY: " << ToString(fn->ret) << std::endl;
  } else {
    // var
    auto var = reinterpret_cast<DefVar *>(def.get());
    std::cout << "KIND: " << (var->isLet ? "let" : "var") << std::endl;
    std::cout << "TY: " << ToString(var->ty) << std::endl;
  }
  std::cout << "NAME: " << def->name << std::endl;
  std::cout << "DEPTH: " << unsigned(def->depth) << std::endl;
}

void DefTable::PrintGlobal() { global_.Print(); }

void DefTable::PrintLocal() { local_.Print(); }

const Def *DefTable::Get(DefId defId) {
  auto def = global_.Get(defId);
  if (def != nullptr) return def;
  def = local_.Get(defId);
  if (def != nullptr) return def;
  return nullptr;
}

void Stack::Print() {
  std::cout << "--------------" << std::endl;
  for (auto const &def : defs_) {
    printDef(def);
    std::cout << "--------------" << std::endl;
  }
}

DefFn *GlobalStack::FindFn(uint8_t depth, std::string name) {
  return (DefFn *)Find(true, depth, name);
}

DefVar *GlobalStack::FindVar(uint8_t depth, std::string name) {
  return (DefVar *)Find(false, depth, name);
}

const Def *GlobalStack::Find(bool isFn, uint8_t depth, std::string name) {
  auto it = std::find_if(defs_.begin(), defs_.end(),
                         [isFn, depth, name](const std::unique_ptr<Def> &def) {
                           return isFn == def->isFn() && def->depth <= depth &&
                                  def->name == name;
                         });
  if (it != defs_.end()) {
    return it->get();
  } else {
    return nullptr;
  }
}

void LocalStack::Push() { base_ = defs_.size(); }

void LocalStack::Pop() {
  defs_.erase(defs_.begin() + base_, defs_.end());
  base_ = 0;
}

DefFn *LocalStack::FindFn(uint8_t depth, std::string name) {
  return (DefFn *)Find(true, depth, name);
}

DefVar *LocalStack::FindVar(uint8_t depth, std::string name) {
  return (DefVar *)Find(false, depth, name);
}

const Def *LocalStack::Find(bool isFn, uint8_t depth, std::string name) {
  auto it = std::find_if(defs_.rbegin(), defs_.rend(),
                         [isFn, depth, name](const std::unique_ptr<Def> &def) {
                           return isFn == def->isFn() && def->depth <= depth &&
                                  def->name == name;
                         });
  if (it != defs_.rend()) {
    return it->get();
  } else {
    return nullptr;
  }
}

const Def *Stack::Get(DefId id) {
  auto it = std::find_if(
      defs_.begin(), defs_.end(),
      [id](const std::unique_ptr<Def> &def) { return def->id == id; });
  if (it != defs_.end()) {
    return it->get();
  } else {
    return nullptr;
  }
}

}  // namespace felis
