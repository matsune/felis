#ifndef FELIS_CHECK_CHECK_H_
#define FELIS_CHECK_CHECK_H_

#include <syntax/ast.h>

#include <map>
#include <memory>
#include <string>
#include <vector>

namespace felis {

struct Type {
  enum Kind {
    // FuncType
    FUNC,
    // BasicType
    VOID,
    I32,
    BOOL,
    CHAR,
  };
  virtual Kind kind() = 0;
};

struct FuncType : public Type {
  Type::Kind kind() { return Type::Kind::FUNC; }

  std::vector<std::shared_ptr<Type>> args;
  std::shared_ptr<Type> ret;
};

struct BasicType : public Type {
  BasicType(Type::Kind kind) : kind_(kind){};

  Type::Kind kind() { return kind_; }

 private:
  Type::Kind kind_;
};

struct Decl {
  enum Kind { EXT, FN, ARG, VAR, LET };
  std::string name;
  std::unique_ptr<Type> type;
  Kind kind;

  Decl(std::string name, std::unique_ptr<Type> type, Kind kind)
      : name(name), type(std::move(type)), kind(kind) {}

  bool IsAssignable() {
    switch (kind) {
      case Kind::EXT:
      case Kind::FN:
      case Kind::VAR:
        return false;
      default:
        return true;
    }
  }
};

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
    typeMap_.emplace(name, type);
  }

  std::shared_ptr<Scope> GetParent() { return parent_; }

  bool IsTop() { return parent_ == nullptr; }

 private:
  std::shared_ptr<Scope> parent_;
  std::map<std::string, std::shared_ptr<Decl>> declMap_;
  std::map<std::string, std::shared_ptr<Type>> typeMap_;
};

class Checker {
 public:
  Checker() : currentScope_(std::make_shared<Scope>(nullptr)){};
  void SetupBuiltin();
  void Check(std::unique_ptr<File>&);

 private:
  std::shared_ptr<Scope> currentScope_;

  std::shared_ptr<Decl> InsertFnDecl(bool isExt,
                                     const std::unique_ptr<FnProto>& proto);

  void OpenScope();
  void CloseScope();
  bool CanDecl(std::string name);
  std::shared_ptr<Decl> LookupDecl(std::string name);
  std::shared_ptr<Type> LookupType(std::string name);
};

}  // namespace felis

#endif  // FELIS_CHECK_CHECK_H_
