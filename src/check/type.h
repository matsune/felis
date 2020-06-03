#ifndef FELIS_CHECK_TYPE_H_
#define FELIS_CHECK_TYPE_H_

#include <cassert>
#include <memory>
#include <string>
#include <vector>

#include "macro.h"

namespace felis {

struct Type {
  enum Kind { FIXED, UNTYPED };
  virtual inline Type::Kind TypeKind() const = 0;

  virtual inline bool IsVoid() const { return false; }
  virtual inline bool IsI8() const { return false; }
  virtual inline bool IsI16() const { return false; }
  virtual inline bool IsI32() const { return false; }
  virtual inline bool IsI64() const { return false; }
  virtual inline bool IsF32() const { return false; }
  virtual inline bool IsF64() const { return false; }
  virtual inline bool IsBool() const { return false; }
  virtual inline bool IsFunc() const { return false; }
  virtual inline bool IsArray() const { return false; }
  virtual inline bool IsString() const { return false; }
  virtual inline bool IsPtr() const { return false; }

  virtual inline bool IsUnresolved() const { return false; }
  virtual inline bool IsUntypedInt() const { return false; }
  virtual inline bool IsUntypedFloat() const { return false; }

  inline bool IsFixed() const { return TypeKind() == Kind::FIXED; }
  inline bool IsUntyped() const { return TypeKind() == Kind::UNTYPED; }

  bool operator==(const Type& other) const;
  bool operator!=(const Type& other) const { return !(*this == other); }

  inline bool IsFixedInt() const {
    return IsI8() || IsI16() || IsI32() || IsI64();
  }
  inline bool IsFixedFloat() const { return IsF32() || IsF64(); }
  inline bool IsFixedNumeric() const { return IsFixedInt() || IsFixedFloat(); }
};

struct FixedType : public Type {
  enum Kind {
    // primitive
    VOID,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    BOOL,
    STRING,
    // compound
    FUNC,
    ARRAY,
    PTR
  };

  FixedType::Kind kind;

  FixedType(FixedType::Kind kind) : kind(kind) {}

  inline Type::Kind TypeKind() const override { return Type::Kind::FIXED; }

  bool operator==(const FixedType& other) const;

  inline bool IsVoid() const override { return kind == FixedType::Kind::VOID; }
  inline bool IsI8() const override { return kind == FixedType::Kind::I8; }
  inline bool IsI16() const override { return kind == FixedType::Kind::I16; }
  inline bool IsI32() const override { return kind == FixedType::Kind::I32; }
  inline bool IsI64() const override { return kind == FixedType::Kind::I64; }
  inline bool IsF32() const override { return kind == FixedType::Kind::F32; }
  inline bool IsF64() const override { return kind == FixedType::Kind::F64; }
  inline bool IsBool() const override { return kind == FixedType::Kind::BOOL; }
  inline bool IsString() const override {
    return kind == FixedType::Kind::STRING;
  }

  inline bool IsFunc() const override { return kind == FixedType::Kind::FUNC; }

  inline bool IsArray() const override {
    return kind == FixedType::Kind::ARRAY;
  }

  inline bool IsPtr() const override { return kind == FixedType::Kind::PTR; }
};

struct FuncType : public FixedType {
  FuncType(std::vector<std::shared_ptr<Type>> args, std::shared_ptr<Type> ret)
      : FixedType(FixedType::Kind::FUNC), args(args), ret(ret) {}

  bool operator==(const FuncType& other) const {
    if (args.size() != other.args.size()) return false;
    for (auto i = 0; i < args.size(); i++) {
      if (*args.at(i) != *other.args.at(i)) return false;
    }
    if (ret) {
      if (!other.ret) return false;
      return *ret == *other.ret;
    } else {
      if (other.ret) return false;
      return true;
    }
  }

  std::vector<std::shared_ptr<Type>> args;
  std::shared_ptr<Type> ret;
};

struct ArrayType : public FixedType {
  ArrayType(std::shared_ptr<Type> elem, int64_t size)
      : FixedType(FixedType::Kind::ARRAY), elem(elem), size(size) {}

  bool operator==(const ArrayType& other) const {
    return *elem == *other.elem && size == other.size;
  }

  std::shared_ptr<Type> elem;
  int64_t size;
};

struct PtrType : public FixedType {
  PtrType(std::shared_ptr<Type> elem)
      : FixedType(FixedType::Kind::PTR), elem(elem) {}

  bool operator==(const PtrType& other) const { return *elem == *other.elem; }

  std::shared_ptr<Type> elem;
};

struct Untyped : public Type {
  enum Kind {
    UNRESOLVED,
    INT,
    FLOAT,
  };

  Untyped::Kind UntypedKind() const { return kind; }

  Untyped(Untyped::Kind kind) : kind(kind), ref(nullptr) {}

  Type::Kind TypeKind() const override { return Type::Kind::UNTYPED; }

  inline bool IsUnresolved() const override {
    return kind == Untyped::Kind::UNRESOLVED;
  }

  inline bool IsUntypedInt() const override {
    return kind == Untyped::Kind::INT;
  }

  inline bool IsUntypedFloat() const override {
    return kind == Untyped::Kind::FLOAT;
  }

  // To check a type can be another type implicitly.
  //
  // ex.) let a = 4 + 3.2
  //
  // `4` will be parsed as untyped int and `3.2` will be parsed
  // as untyped float. As lhs and rhs should be same type under
  // the binary expression rule, 4 and 3.2 will be same type.
  // Untyped float cannot be int type implicitly but int type
  // can be float.
  bool Canbe(std::shared_ptr<Type> to) const {
    assert(ref == nullptr);
    switch (kind) {
      case Untyped::Kind::UNRESOLVED:
        return true;
      case Untyped::Kind::INT:
        return to->IsUntyped() || to->IsFixedNumeric();
      case Untyped::Kind::FLOAT:
        return to->IsUntypedFloat() || to->IsFixedFloat();
    }
  }

  const std::shared_ptr<Type>& Ref() const { return ref; }

  void SetRef(std::shared_ptr<Type> to) { ref = to; }

 private:
  Untyped::Kind kind;

  // `ref` means that this type depends on ref's type.
  //
  // ex.)
  //  let a = 4
  //  let b: i64 = a
  //
  // First of all, we put unknown types `T` for each expression.
  //  4 : T1
  //  a : T2
  //  b : T3
  //
  // Since an VarDecl statement should have a same type for left and right
  // expressions, it assures T2 = T1. In other words, T1 is untyped int but
  // it depends on T2 so we set T2 into T1's ref.
  // The second statement also assures T2's ref is T3. But T3 type is declared
  // as concrete type `i64` explicitly. As a result, we can infer all of these
  // types T1 = T2 = T3 = i64.
  //
  std::shared_ptr<Type> ref;
};

const auto kTypeVoid = std::make_shared<FixedType>(FixedType::Kind::VOID);
const auto kTypeI8 = std::make_shared<FixedType>(FixedType::Kind::I8);
const auto kTypeI16 = std::make_shared<FixedType>(FixedType::Kind::I16);
const auto kTypeI32 = std::make_shared<FixedType>(FixedType::Kind::I32);
const auto kTypeI64 = std::make_shared<FixedType>(FixedType::Kind::I64);
const auto kTypeF32 = std::make_shared<FixedType>(FixedType::Kind::F32);
const auto kTypeF64 = std::make_shared<FixedType>(FixedType::Kind::F64);
const auto kTypeBool = std::make_shared<FixedType>(FixedType::Kind::BOOL);
const auto kTypeString = std::make_shared<FixedType>(FixedType::Kind::STRING);
std::shared_ptr<Untyped> Unresolved();
std::shared_ptr<Untyped> UntypedInt();
std::shared_ptr<Untyped> UntypedFloat();
std::shared_ptr<FixedType> ArchInt(bool is_32bit);

std::shared_ptr<Type> Underlying(std::shared_ptr<Type> ty);

std::shared_ptr<Type> FinalType(std::shared_ptr<Type> ty, bool is_32bit);

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_H_
