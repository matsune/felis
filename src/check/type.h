#ifndef FELIS_CHECK_TYPE_H_
#define FELIS_CHECK_TYPE_H_

#include <cassert>
#include <memory>
#include <string>
#include <vector>

namespace felis {

struct Ty {
  enum Kind { UNTYPED, TYPED };
  virtual Ty::Kind TyKind() const = 0;

  bool IsTyped() const { return TyKind() == Kind::TYPED; }
  bool IsUntyped() const { return TyKind() == Kind::UNTYPED; }

  bool operator==(const Ty& other);
  bool operator!=(const Ty& other) { return !(*this == other); }

  virtual bool IsFunc() const { return false; }
  virtual bool IsBool() const { return false; }
  virtual bool IsString() const { return false; }
  virtual bool IsVoid() const { return false; }
  virtual bool IsI8() const { return false; }
  virtual bool IsI16() const { return false; }
  virtual bool IsI32() const { return false; }
  virtual bool IsI64() const { return false; }
  virtual bool IsF32() const { return false; }
  virtual bool IsF64() const { return false; }
  virtual bool IsTypedInt() const { return false; }
  virtual bool IsTypedFloat() const { return false; }
  virtual bool IsTypedNum() const { return false; }

  virtual bool IsUntypedInt() const { return false; }
  virtual bool IsUntypedFloat() const { return false; }
};

struct Typed : public Ty {
  enum Kind {
    // FuncType
    FUNC,
    // BasicType
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
  Typed::Kind kind;

  Typed::Kind TypedKind() const { return kind; }

  Typed(Typed::Kind kind) : kind(kind) {}

  Ty::Kind TyKind() const override { return Ty::TYPED; }

  bool IsFunc() const override { return kind == Typed::Kind::FUNC; }
  bool IsBool() const override { return kind == Typed::Kind::BOOL; }
  bool IsString() const override { return kind == Typed::Kind::STRING; }
  bool IsVoid() const override { return kind == Typed::Kind::VOID; }
  bool IsI8() const override { return kind == Typed::Kind::I8; }
  bool IsI16() const override { return kind == Typed::Kind::I16; }
  bool IsI32() const override { return kind == Typed::Kind::I32; }
  bool IsI64() const override { return kind == Typed::Kind::I64; }
  bool IsF32() const override { return kind == Typed::Kind::F32; }
  bool IsF64() const override { return kind == Typed::Kind::F64; }
  bool IsTypedInt() const override {
    return IsI8() || IsI16() || IsI32() || IsI64();
  }
  bool IsTypedFloat() const override { return IsF32() || IsF64(); }
  bool IsTypedNum() const override { return IsTypedInt() || IsTypedFloat(); }
};

struct FuncType : public Typed {
  FuncType(std::vector<std::shared_ptr<Typed>> args, std::shared_ptr<Typed> ret)
      : Typed(Typed::Kind::FUNC), args(args), ret(ret) {}

  std::vector<std::shared_ptr<Typed>> args;
  std::shared_ptr<Typed> ret;
};

struct Untyped : public Ty {
  enum Kind {
    /* UNRESOLVED, */
    INT,
    FLOAT,
  };

  Untyped::Kind UntypedKind() const { return kind; }

  Untyped(Untyped::Kind kind) : kind(kind), ref(nullptr) {}

  Ty::Kind TyKind() const override { return Ty::UNTYPED; }

  bool IsUntypedInt() const override { return kind == Untyped::Kind::INT; }
  bool IsUntypedFloat() const override { return kind == Untyped::Kind::FLOAT; }

  bool Canbe(std::shared_ptr<Ty> to) const {
    assert(ref == nullptr);
    switch (kind) {
      case Untyped::Kind::INT:
        return to->IsUntyped() || to->IsTypedNum();
      case Untyped::Kind::FLOAT:
        return to->IsUntypedFloat() || to->IsTypedFloat();
    }
  }

  void SetRef(std::shared_ptr<Ty> ty) {
    assert(ref == nullptr);
    ref = ty;
  }

  const std::shared_ptr<Ty>& GetRef() const { return ref; }

 private:
  Untyped::Kind kind;
  std::shared_ptr<Ty> ref;
};

const auto kTypeVoid = std::make_shared<Typed>(Typed::Kind::VOID);
const auto kTypeI8 = std::make_shared<Typed>(Typed::Kind::I8);
const auto kTypeI16 = std::make_shared<Typed>(Typed::Kind::I16);
const auto kTypeI32 = std::make_shared<Typed>(Typed::Kind::I32);
const auto kTypeI64 = std::make_shared<Typed>(Typed::Kind::I64);
const auto kTypeF32 = std::make_shared<Typed>(Typed::Kind::F32);
const auto kTypeF64 = std::make_shared<Typed>(Typed::Kind::F64);
const auto kTypeBool = std::make_shared<Typed>(Typed::Kind::BOOL);
const auto kTypeString = std::make_shared<Typed>(Typed::Kind::STRING);
std::shared_ptr<Untyped> UntypedInt();
std::shared_ptr<Untyped> UntypedFloat();

std::shared_ptr<Ty> FinalTy(const std::shared_ptr<Ty>&);

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_H_
