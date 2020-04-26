#ifndef FELIS_IR_SYMTAB_H_
#define FELIS_IR_SYMTAB_H_

#include <llvm/IR/Function.h>

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "type.h"

namespace felis {

struct Def {
  enum Kind { DEF_FN, DEF_ARG, DEF_VAR, DEF_LET };

  std::string name;

  Def(std::string name) : name(name){};

  virtual Kind DefKind() = 0;
};

struct DefFn : Def {
  bool isExt;
  std::vector<Ty> args;
  Ty ret = Ty::UNKNOWN;
  llvm::Function *func;

  DefFn(std::string name) : Def(name){};

  Kind DefKind() override { return DEF_FN; }
};

struct DefVariable : Def {
  Ty ty;

  DefVariable(std::string name, Ty ty) : Def(name), ty(ty) {}

  virtual bool IsMut() = 0;
  virtual llvm::Value *Value() = 0;
};

struct DefArg : DefVariable {
  llvm::Argument *arg;

  DefArg(std::string name, Ty ty) : DefVariable(name, ty) {}

  Kind DefKind() override { return DEF_ARG; }
  bool IsMut() override { return false; }
  llvm::Value *Value() override { return arg; }
};

struct DefVar : DefVariable {
  DefVar(std::string name, Ty ty) : DefVariable(name, ty) {}
  llvm::Value *value;

  Kind DefKind() override { return DEF_VAR; }
  bool IsMut() override { return true; }
  llvm::Value *Value() override { return value; }
};

struct DefLet : DefVariable {
  DefLet(std::string name, Ty ty) : DefVariable(name, ty) {}
  llvm::Value *value;

  Kind DefKind() override { return DEF_LET; }
  bool IsMut() override { return false; }
  llvm::Value *Value() override { return value; }
};

class SymTab {
 public:
  SymTab(std::shared_ptr<SymTab> parent) : parent_(std::move(parent)){};

  std::shared_ptr<DefFn> FindFn(std::string name) {
    auto it = fnMap_.find(name);
    if (it != fnMap_.end()) return it->second;
    return nullptr;
  }

  void InsertFn(std::string name, std::shared_ptr<DefFn> def) {
    fnMap_.emplace(name, def);
  }

  std::shared_ptr<DefVariable> FindVariable(std::string name) {
    auto it = variableMap_.find(name);
    if (it != variableMap_.end()) return it->second;
    return nullptr;
  }

  void InsertVariable(std::string name, std::shared_ptr<DefVariable> def) {
    variableMap_.emplace(name, def);
  }

  std::shared_ptr<SymTab> GetParent() { return parent_; }

  bool IsTop() { return parent_ == nullptr; }

 private:
  std::shared_ptr<SymTab> parent_;
  std::map<std::string, std::shared_ptr<DefFn>> fnMap_;
  std::map<std::string, std::shared_ptr<DefVariable>> variableMap_;
};

class SymTabManager {
 public:
  SymTabManager() : current_(std::make_shared<SymTab>(nullptr)){};

  void Push() { current_ = std::make_shared<SymTab>(current_); }

  void Pop() {
    if (current_->IsTop()) throw std::runtime_error("already top scope");
    current_ = current_->GetParent();
  }

  void InsertFn(std::string name, std::shared_ptr<DefFn> def) {
    current_->InsertFn(name, def);
  }

  void InsertVariable(std::string name, std::shared_ptr<DefVariable> def) {
    current_->InsertVariable(name, def);
  }

  std::shared_ptr<DefFn> LookupFn(std::string name) {
    auto &scope = current_;
    while (scope) {
      auto def = scope->FindFn(name);
      if (def) return def;
      scope = scope->GetParent();
    }
    return nullptr;
  }

  std::shared_ptr<DefVariable> LookupVariable(std::string name) {
    std::shared_ptr<SymTab> scope = current_;
    while (scope) {
      auto def = scope->FindVariable(name);
      if (def) return def;
      scope = scope->GetParent();
    }
    return nullptr;
  }

  bool CanDeclareFn(std::string name) {
    return current_->FindFn(name) == nullptr;
  }

  bool CanDeclareVariable(std::string name) {
    return current_->FindVariable(name) == nullptr;
  }

 private:
  std::shared_ptr<SymTab> current_;
};

}  // namespace felis

#endif  // FELIS_IR_SYMTAB_H_
