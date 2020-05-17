#ifndef FELIS_CHECK_TYPE_H_
#define FELIS_CHECK_TYPE_H_

#include <memory>
#include <string>
#include <vector>

namespace felis {

struct Type {
 public:
  enum Kind {
    UNRESOLVED,
    // FuncType
    FUNC,
    // BasicType
    VOID,
    I32,
    I64,
    F32,
    F64,
    BOOL,
    CHAR,
    STRING,
  };

  Type(Type::Kind kind) : kind_(kind){};
  Type::Kind TypeKind() { return kind_; };
  bool IsUnresolved() { return kind_ == Type::Kind::UNRESOLVED; }
  bool IsBool() { return kind_ == Type::Kind::BOOL; }
  bool IsI32() { return kind_ == Type::Kind::I32; }
  bool IsI64() { return kind_ == Type::Kind::I64; }
  bool IsInt() { return IsI32() || IsI64(); }
  bool IsF32() { return kind_ == Type::Kind::F32; }
  bool IsF64() { return kind_ == Type::Kind::F64; }
  bool IsFloat() { return IsF32() || IsF64(); }
  bool IsChar() { return kind_ == Type::Kind::CHAR; }
  bool IsString() { return kind_ == Type::Kind::STRING; }
  bool IsVoid() { return kind_ == Type::Kind::VOID; }

  bool IsNumeric() { return IsInt() || IsFloat() || IsChar(); }

  friend bool operator==(const Type& lhs, const Type& rhs) {
    return lhs.kind_ == rhs.kind_;
  }

  friend bool operator!=(const Type& lhs, const Type& rhs) {
    return !(lhs == rhs);
  }

 private:
  Type::Kind kind_;
};

struct FuncType : public Type {
  FuncType(std::vector<std::shared_ptr<Type>> args, std::shared_ptr<Type> ret)
      : Type(Type::Kind::FUNC), args(std::move(args)), ret(std::move(ret)) {}

  std::vector<std::shared_ptr<Type>> args;
  std::shared_ptr<Type> ret;
};

const auto kTypeUnresolved = std::make_shared<Type>(Type::Kind::UNRESOLVED);
const auto kTypeVoid = std::make_shared<Type>(Type::Kind::VOID);
const auto kTypeI32 = std::make_shared<Type>(Type::Kind::I32);
const auto kTypeI64 = std::make_shared<Type>(Type::Kind::I64);
const auto kTypeF32 = std::make_shared<Type>(Type::Kind::F32);
const auto kTypeF64 = std::make_shared<Type>(Type::Kind::F64);
const auto kTypeBool = std::make_shared<Type>(Type::Kind::BOOL);
const auto kTypeChar = std::make_shared<Type>(Type::Kind::CHAR);
const auto kTypeString = std::make_shared<Type>(Type::Kind::STRING);

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_H_
