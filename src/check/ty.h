#ifndef FELIS_CHECK_TY_H_
#define FELIS_CHECK_TY_H_

#include <cassert>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

namespace felis {

struct PtrTy;

struct Ty {
  enum Kind {
    // untyped
    UNRESOLVED,
    UNTYPED_INT,
    UNTYPED_FLOAT,
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

  Ty::Kind kind;

  Ty(Ty::Kind kind) : kind(kind){};
  virtual ~Ty(){};

  bool operator==(const Ty &other) const;
  bool operator!=(const Ty &other) const { return !(*this == other); }

  bool IsUnResolved() const { return kind == Ty::Kind::UNRESOLVED; }
  bool IsUntypedInt() const { return kind == Ty::Kind::UNTYPED_INT; }
  bool IsUntypedFloat() const { return kind == Ty::Kind::UNTYPED_FLOAT; }
  bool IsVoid() const { return kind == Ty::Kind::VOID; }
  bool IsI8() const { return kind == Ty::Kind::I8; }
  bool IsI16() const { return kind == Ty::Kind::I16; }
  bool IsI32() const { return kind == Ty::Kind::I32; }
  bool IsI64() const { return kind == Ty::Kind::I64; }
  bool IsF32() const { return kind == Ty::Kind::F32; }
  bool IsF64() const { return kind == Ty::Kind::F64; }
  bool IsBool() const { return kind == Ty::Kind::BOOL; }
  bool IsString() const { return kind == Ty::Kind::STRING; }
  bool IsFunc() const { return kind == Ty::Kind::FUNC; }
  bool IsArray() const { return kind == Ty::Kind::ARRAY; }
  bool IsPtr() const { return kind == Ty::Kind::PTR; }

  bool IsUntyped() const {
    return kind == Ty::Kind::UNRESOLVED || kind == Ty::Kind::UNTYPED_INT ||
           kind == Ty::Kind::UNTYPED_FLOAT;
  }

  bool IsInt() const {
    return kind == Ty::Kind::I8 || kind == Ty::Kind::I16 ||
           kind == Ty::Kind::I32 || kind == Ty::Kind::I64;
  }

  bool IsFloat() const {
    return kind == Ty::Kind::F32 || kind == Ty::Kind::F64;
  }

  bool IsNum() const { return IsInt() || IsFloat(); }

  bool IsPrimitive() const {
    return kind == Ty::Kind::I8 || kind == Ty::Kind::I16 ||
           kind == Ty::Kind::I32 || kind == Ty::Kind::I64 ||
           kind == Ty::Kind::F32 || kind == Ty::Kind::F64 ||
           kind == Ty::Kind::BOOL;
  }

  std::shared_ptr<Ty> GetPtrElement();
};

struct FuncTy : Ty {
  std::vector<std::shared_ptr<Ty>> args;
  std::shared_ptr<Ty> ret;

  FuncTy(std::vector<std::shared_ptr<Ty>> args, std::shared_ptr<Ty> ret)
      : Ty(Ty::Kind::FUNC), args(args), ret(ret) {
    assert(ret);
  };
};

struct ArrayTy : Ty {
  std::shared_ptr<Ty> elem;
  int size;

  ArrayTy(std::shared_ptr<Ty> elem, int size)
      : Ty(Ty::Kind::ARRAY), elem(elem), size(size){};
};

struct PtrTy : Ty {
  std::shared_ptr<Ty> ref;

  PtrTy(std::shared_ptr<Ty> ref) : Ty(Ty::Kind::PTR), ref(ref){};
};

const auto kTypeVoid = std::make_shared<Ty>(Ty::Kind::VOID);
const auto kTypeI8 = std::make_shared<Ty>(Ty::Kind::I8);
const auto kTypeI16 = std::make_shared<Ty>(Ty::Kind::I16);
const auto kTypeI32 = std::make_shared<Ty>(Ty::Kind::I32);
const auto kTypeI64 = std::make_shared<Ty>(Ty::Kind::I64);
const auto kTypeF32 = std::make_shared<Ty>(Ty::Kind::F32);
const auto kTypeF64 = std::make_shared<Ty>(Ty::Kind::F64);
const auto kTypeBool = std::make_shared<Ty>(Ty::Kind::BOOL);
const auto kTypeString = std::make_shared<Ty>(Ty::Kind::STRING);

std::shared_ptr<Ty> ArchInt(bool);
std::shared_ptr<Ty> UnResolved();
std::shared_ptr<Ty> UntypedInt();
std::shared_ptr<Ty> UntypedFloat();

std::shared_ptr<Ty> ToPtr(std::shared_ptr<Ty>);

}  // namespace felis

#endif  // FELIS_CHECK_TY_H_
