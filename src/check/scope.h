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
  Scope(std::shared_ptr<Scope> parent) : parent_(parent){};

  std::shared_ptr<Decl> FindDecl(const std::string &name) {
    auto it = decl_map_.find(name);
    if (it != decl_map_.end()) return it->second;
    return nullptr;
  }

  void InsertDecl(std::string name, std::shared_ptr<Decl> decl) {
    decl_map_.emplace(name, decl);
  }

  const std::shared_ptr<Type> FindType(const std::string &name) {
    auto it = type_map_.find(name);
    if (it != type_map_.end()) return it->second;
    return nullptr;
  }

  void InsertType(std::string name, std::shared_ptr<Type> type) {
    type_map_.emplace(name, type);
  }

  std::shared_ptr<Scope> GetParent() { return parent_; }

  bool IsTop() { return parent_ == nullptr; }

 private:
  std::shared_ptr<Scope> parent_;
  std::map<std::string, std::shared_ptr<Decl>> decl_map_;
  std::map<std::string, std::shared_ptr<Type>> type_map_;
};

}  // namespace felis

#endif  // FELIS_CHECK_SCOPE_H_
