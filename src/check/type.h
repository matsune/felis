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
    // Untyped
    UNTYPED_INT,
    UNTYPED_FLOAT,
    // FuncType
    FUNC,
    // BasicType
    VOID,
    I32,
    I64,
    F32,
    F64,
    BOOL,
    STRING,
  };

  Type(Type::Kind kind) : kind_(kind){};
  Type::Kind TypeKind() const { return kind_; };

  bool IsUnresolved() const { return kind_ == Type::Kind::UNRESOLVED; }
  bool IsUntyped() const {
    return kind_ == Type::Kind::UNTYPED_INT ||
           kind_ == Type::Kind::UNTYPED_FLOAT;
  }
  bool IsUntypedFloat() const { return kind_ == Type::Kind::UNTYPED_FLOAT; }

  bool IsBool() const { return kind_ == Type::Kind::BOOL; }
  bool IsString() const { return kind_ == Type::Kind::STRING; }
  bool IsVoid() const { return kind_ == Type::Kind::VOID; }
  bool IsI32() const { return kind_ == Type::Kind::I32; }
  bool IsI64() const { return kind_ == Type::Kind::I64; }
  bool IsF32() const { return kind_ == Type::Kind::F32; }
  bool IsF64() const { return kind_ == Type::Kind::F64; }
  bool IsTypedInt() const { return IsI32() || IsI64(); }
  bool IsTypedFloat() const { return IsF32() || IsF64(); }
  bool IsNumeric() const { return IsTypedInt() || IsTypedFloat(); }
  bool IsSolid() const { return !IsUnresolved() && !IsUntyped(); }

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

struct UnresolvedType : public Type {
  UnresolvedType(uint64_t id) : Type(Type::Kind::UNRESOLVED), id(id) {}
  uint64_t id;
};

/* const auto kTypeUnresolved = std::make_shared<Type>(Type::Kind::UNRESOLVED);
 */
const auto kTypeVoid = std::make_shared<Type>(Type::Kind::VOID);
const auto kTypeI32 = std::make_shared<Type>(Type::Kind::I32);
const auto kTypeI64 = std::make_shared<Type>(Type::Kind::I64);
const auto kTypeF32 = std::make_shared<Type>(Type::Kind::F32);
const auto kTypeF64 = std::make_shared<Type>(Type::Kind::F64);
const auto kTypeBool = std::make_shared<Type>(Type::Kind::BOOL);
/* const auto kTypeChar = std::make_shared<Type>(Type::Kind::CHAR); */
const auto kTypeString = std::make_shared<Type>(Type::Kind::STRING);

std::shared_ptr<Type> MakeUnresolved();
std::shared_ptr<Type> MakeUntypedInt();
std::shared_ptr<Type> MakeUntypedFloat();

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_H_
