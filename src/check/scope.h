#ifndef FELIS_CHECK_SCOPE_H_
#define FELIS_CHECK_SCOPE_H_

#include <iostream>
#include <map>
#include <memory>
#include <string>

#include "check/decl.h"
#include "check/type.h"
#include "string/string.h"

namespace felis {

class Scope {
 public:
  Scope(std::shared_ptr<Scope> parent) : parent_(std::move(parent)){};

  std::shared_ptr<Decl> FindDecl(std::string name) {
    auto it = declMap_.find(name);
    if (it != declMap_.end()) return it->second;
    return nullptr;
  }

  void InsertDecl(std::string name, std::shared_ptr<Decl> decl) {
    declMap_.emplace(name, decl);
  }

  std::shared_ptr<Type> FindType(std::string name) {
    auto it = typeMap_.find(name);
    if (it != typeMap_.end()) return it->second;
    return nullptr;
  }

  void InsertType(std::string name, std::shared_ptr<Type> type) {
    typeMap_.emplace(name, std::move(type));
  }

  std::shared_ptr<Scope> GetParent() { return parent_; }

  bool IsTop() { return parent_ == nullptr; }

  void Debug() {
    for (auto &it : declMap_) {
      std::cout << it.first << ":" << ToString(it.second->kind) << std::endl;
    }
  }

 private:
  std::shared_ptr<Scope> parent_;
  std::map<std::string, std::shared_ptr<Decl>> declMap_;
  std::map<std::string, std::shared_ptr<Type>> typeMap_;
};

}  // namespace felis

#endif  // FELIS_CHECK_SCOPE_H_
