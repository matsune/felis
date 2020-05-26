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

  virtual inline bool IsPrim() const { return false; }
  virtual inline bool IsFunc() const { return false; }

  virtual inline bool IsUnresolved() const { return false; }
  virtual inline bool IsUntypedInt() const { return false; }
  virtual inline bool IsUntypedFloat() const { return false; }

  virtual inline bool IsVoid() const { return false; }
  virtual inline bool IsI8() const { return false; }
  virtual inline bool IsI16() const { return false; }
  virtual inline bool IsI32() const { return false; }
  virtual inline bool IsI64() const { return false; }
  virtual inline bool IsF32() const { return false; }
  virtual inline bool IsF64() const { return false; }
  virtual inline bool IsBool() const { return false; }
  virtual inline bool IsString() const { return false; }

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
  enum Kind { PRIM, FUNC };
  virtual inline FixedType::Kind FixedKind() const = 0;

  inline Type::Kind TypeKind() const override { return Type::Kind::FIXED; }

  bool operator==(const FixedType& other) const;

  inline bool IsPrim() const override {
    return FixedKind() == FixedType::Kind::PRIM;
  }

  inline bool IsFunc() const override {
    return FixedKind() == FixedType::Kind::FUNC;
  }
};

struct PrimType : public FixedType {
  enum Kind {
    VOID,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    BOOL,
    STRING,
  };
  PrimType::Kind kind;

  PrimType::Kind TypedKind() const { return kind; }

  PrimType(PrimType::Kind kind) : kind(kind) {}

  bool operator==(const PrimType& other) const { return kind == other.kind; }

  inline virtual FixedType::Kind FixedKind() const override {
    return FixedType::Kind::PRIM;
  }

  inline bool IsVoid() const override { return kind == PrimType::Kind::VOID; }
  inline bool IsI8() const override { return kind == PrimType::Kind::I8; }
  inline bool IsI16() const override { return kind == PrimType::Kind::I16; }
  inline bool IsI32() const override { return kind == PrimType::Kind::I32; }
  inline bool IsI64() const override { return kind == PrimType::Kind::I64; }
  inline bool IsF32() const override { return kind == PrimType::Kind::F32; }
  inline bool IsF64() const override { return kind == PrimType::Kind::F64; }
  inline bool IsBool() const override { return kind == PrimType::Kind::BOOL; }
  inline bool IsString() const override {
    return kind == PrimType::Kind::STRING;
  }
};

struct FuncType : public FixedType {
  FuncType(std::vector<std::shared_ptr<Type>> args, std::shared_ptr<Type> ret)
      : args(args), ret(ret) {}

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

  FixedType::Kind FixedKind() const override { return FixedType::Kind::FUNC; }

  std::vector<std::shared_ptr<Type>> args;
  std::shared_ptr<Type> ret;
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

  bool TryResolve(std::shared_ptr<Type> ty) {
    assert(ref == nullptr);
    if (Canbe(ty)) {
      ref = ty;
      return true;
    }
    return false;
  }

  const std::shared_ptr<Type>& Ref() const { return ref; }

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

/* struct Aliased : public Type { */
/*   Type::Kind TypeKind() const override { return Type::Kind::ALIASED; } */

/*  private: */
/*   std::shared_ptr<Type> ref; */
/* }; */

const auto kTypeVoid = std::make_shared<PrimType>(PrimType::Kind::VOID);
const auto kTypeI8 = std::make_shared<PrimType>(PrimType::Kind::I8);
const auto kTypeI16 = std::make_shared<PrimType>(PrimType::Kind::I16);
const auto kTypeI32 = std::make_shared<PrimType>(PrimType::Kind::I32);
const auto kTypeI64 = std::make_shared<PrimType>(PrimType::Kind::I64);
const auto kTypeF32 = std::make_shared<PrimType>(PrimType::Kind::F32);
const auto kTypeF64 = std::make_shared<PrimType>(PrimType::Kind::F64);
const auto kTypeBool = std::make_shared<PrimType>(PrimType::Kind::BOOL);
const auto kTypeString = std::make_shared<PrimType>(PrimType::Kind::STRING);
std::shared_ptr<Untyped> Unresolved();
std::shared_ptr<Untyped> UntypedInt();
std::shared_ptr<Untyped> UntypedFloat();
std::shared_ptr<PrimType> ArchInt(bool is_32bit);

std::shared_ptr<Type> Underlying(std::shared_ptr<Type> ty);

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_H_
