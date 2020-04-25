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
    UNKNOWN,
    // FuncType
    FUNC,
    // BasicType
    VOID,
    I32,
    BOOL,
    CHAR,
  };
  Type(Type::Kind kind = Kind::UNKNOWN) : kind_(kind){};
  Kind kind() { return kind_; };
  bool IsUnknown() { return kind_ == Kind::UNKNOWN; }

 private:
  Kind kind_;
};

struct FuncType : public Type {
  FuncType() : Type(Type::Kind::FUNC){};

  std::vector<Type> args;
  Type ret;
};

struct Decl {
  enum Kind { EXT, FN, ARG, VAR, LET };
  std::string name;
  Type type;
  Kind kind;

  Decl(std::string name, Type type, Kind kind)
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

  Type FindType(std::string name) {
    auto it = typeMap_.find(name);
    if (it != typeMap_.end()) return it->second;
    return Type();
  }

  void InsertType(std::string name, Type type) { typeMap_.emplace(name, type); }

  std::shared_ptr<Scope> GetParent() { return parent_; }

  bool IsTop() { return parent_ == nullptr; }

 private:
  std::shared_ptr<Scope> parent_;
  std::map<std::string, std::shared_ptr<Decl>> declMap_;
  std::map<std::string, Type> typeMap_;
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
  void Check(std::unique_ptr<FnDecl>&);

  void OpenScope();
  void CloseScope();
  bool CanDecl(std::string name);
  std::shared_ptr<Decl> LookupDecl(std::string name);
  Type LookupType(std::string name);
};

struct Value {
  enum Kind { CONSTANT, VARIABLE };

  virtual Kind ValueKind() = 0;
  Type type;
};

struct Constant : public Value {
  Value::Kind ValueKind() { return Value::Kind::CONSTANT; }

  enum Kind { INT, BOOL };
  virtual Constant::Kind ConstantKind() = 0;
};

struct IntConstant : public Constant {
  IntConstant(uint64_t val) : val(val){};
  uint64_t val;

  Constant::Kind ConstantKind() { return Constant::Kind::INT; };
};

struct BoolConstant : public Constant {
  BoolConstant(bool val) : val(val){};
  bool val;

  Constant::Kind ConstantKind() { return Constant::Kind::BOOL; };
};

struct Variable : public Value {
  Value::Kind ValueKind() { return Value::Kind::VARIABLE; }
  std::shared_ptr<Decl> decl;
};

}  // namespace felis

#endif  // FELIS_CHECK_CHECK_H_
