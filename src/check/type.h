#ifndef FELIS_CHECK_TYPE_H_
#define FELIS_CHECK_TYPE_H_

#include <cassert>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

namespace felis {

class Type {
 public:
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

  Type(Type::Kind kind) : kind_(kind), ref_(nullptr), ret_(nullptr), size_(0) {}

  bool operator==(const Type&);
  bool operator!=(const Type& other) { return !(*this == other); }

  inline bool IsUnresolved() const { return kind_ == Type::Kind::UNRESOLVED; }
  inline bool IsUntypedInt() const { return kind_ == Type::Kind::UNTYPED_INT; }
  inline bool IsUntypedFloat() const {
    return kind_ == Type::Kind::UNTYPED_FLOAT;
  }

  bool IsUntype() const {
    return IsUnresolved() || IsUntypedInt() || IsUntypedFloat();
  }

  inline bool IsVoid() const { return kind_ == Type::Kind::VOID; }
  inline bool IsI8() const { return kind_ == Type::Kind::I8; }
  inline bool IsI16() const { return kind_ == Type::Kind::I16; }
  inline bool IsI32() const { return kind_ == Type::Kind::I32; }
  inline bool IsI64() const { return kind_ == Type::Kind::I64; }
  inline bool IsF32() const { return kind_ == Type::Kind::F32; }
  inline bool IsF64() const { return kind_ == Type::Kind::F64; }
  inline bool IsBool() const { return kind_ == Type::Kind::BOOL; }
  inline bool IsString() const { return kind_ == Type::Kind::STRING; }

  bool IsConstType() const {
    return IsVoid() || IsI8() || IsI16() || IsI16() || IsI32() || IsI64() ||
           IsF32() || IsF64() || IsBool() || IsString();
  }

  inline bool IsFunc() const { return kind_ == Type::Kind::FUNC; }
  inline bool IsArray() const { return kind_ == Type::Kind::ARRAY; }
  inline bool IsPtr() const { return kind_ == Type::Kind::PTR; }

  bool IsInt() const { return IsI8() || IsI16() || IsI32() || IsI64(); }

  bool IsFloat() const { return IsF32() || IsF64(); }

  bool IsNum() const {
    return IsUntypedInt() || IsUntypedFloat() || IsInt() || IsFloat();
  }

  inline Type::Kind GetKind() const { return kind_; }

  std::shared_ptr<Type> GetRef() const {
    assert(IsUntype());
    return ref_;
  }

  std::shared_ptr<Type> GetRef() {
    assert(IsUntype());
    return ref_;
  }

  void SetRef(std::shared_ptr<Type> ref) {
    assert(IsUntype());
    ref_ = ref;
  }

  std::vector<std::shared_ptr<Type>> GetArgs() const {
    assert(IsFunc());
    return args_;
  }

  std::vector<std::shared_ptr<Type>> GetArgs() {
    assert(IsFunc());
    return args_;
  }

  void SetArgs(std::vector<std::shared_ptr<Type>> args) {
    assert(IsFunc());
    args_ = args;
  }

  std::shared_ptr<Type> GetRet() const {
    assert(IsFunc());
    return ret_;
  }

  std::shared_ptr<Type> GetRet() {
    assert(IsFunc());
    return ret_;
  }

  void SetRet(std::shared_ptr<Type> ret) {
    assert(IsFunc());
    ret_ = ret;
  }

  std::shared_ptr<Type> GetElem() const {
    assert(IsArray() || IsPtr());
    return elem_;
  }

  std::shared_ptr<Type> GetElem() {
    assert(IsArray() || IsPtr());
    return elem_;
  }

  std::shared_ptr<Type> GetPtrElem() const {
    assert(IsPtr());
    return elem_;
  }

  std::shared_ptr<Type> GetPtrElem() {
    assert(IsPtr());
    return elem_;
  }

  std::shared_ptr<Type> GetArrayElem() {
    assert(IsArray());
    return elem_;
  }

  void SetElem(std::shared_ptr<Type> elem) {
    assert(IsArray() || IsPtr());
    elem_ = elem;
  }

  int GetSize() const {
    assert(IsArray());
    return size_;
  }

  void SetSize(int size) {
    assert(IsArray());
    size_ = size;
  }

  static std::shared_ptr<Type> MakeUnresolved() {
    return std::make_shared<Type>(Type::Kind::UNRESOLVED);
  }

  static std::shared_ptr<Type> MakeUntypedInt() {
    return std::make_shared<Type>(Type::Kind::UNTYPED_INT);
  }

  static std::shared_ptr<Type> MakeUntypedFloat() {
    return std::make_shared<Type>(Type::Kind::UNTYPED_FLOAT);
  }

  static std::shared_ptr<Type> MakeI8() {
    return std::make_shared<Type>(Type::Kind::I8);
  }

  static std::shared_ptr<Type> MakeI16() {
    return std::make_shared<Type>(Type::Kind::I16);
  }

  static std::shared_ptr<Type> MakeI32() {
    return std::make_shared<Type>(Type::Kind::I32);
  }

  static std::shared_ptr<Type> MakeI64() {
    return std::make_shared<Type>(Type::Kind::I64);
  }

  static std::shared_ptr<Type> MakeF32() {
    return std::make_shared<Type>(Type::Kind::F32);
  }

  static std::shared_ptr<Type> MakeF64() {
    return std::make_shared<Type>(Type::Kind::F64);
  }

  static std::shared_ptr<Type> MakeVoid() {
    return std::make_shared<Type>(Type::Kind::VOID);
  }

  static std::shared_ptr<Type> MakeBool() {
    return std::make_shared<Type>(Type::Kind::BOOL);
  }

  static std::shared_ptr<Type> MakeString() {
    return std::make_shared<Type>(Type::Kind::STRING);
  }

  static std::shared_ptr<Type> MakeArchInt(bool is_32bit) {
    return std::make_shared<Type>(is_32bit ? Type::Kind::I32 : Type::Kind::I64);
  }

  static std::shared_ptr<Type> MakeFunc(std::vector<std::shared_ptr<Type>> args,
                                        std::shared_ptr<Type> ret) {
    if (!ret) ret = MakeVoid();
    auto func = std::make_shared<Type>(Type::Kind::FUNC);
    func->SetArgs(args);
    func->SetRet(ret);
    return func;
  }

  static std::shared_ptr<Type> MakeArray(std::shared_ptr<Type> elem, int size) {
    auto array = std::make_shared<Type>(Type::Kind::ARRAY);
    array->SetElem(elem);
    array->SetSize(size);
    return array;
  }

  static std::shared_ptr<Type> MakePtr(std::shared_ptr<Type> elem) {
    auto ptr = std::make_shared<Type>(Type::Kind::PTR);
    ptr->SetElem(elem);
    return ptr;
  }

  bool Substitutable(std::shared_ptr<Type>);

  void Constraint(std::shared_ptr<Type>);

  void Resolve(bool);

  bool IsResolved();

 private:
  Type::Kind kind_;

  // UNRESOLVED & UNTYPED_INT & UNTYPED_FLOAT
  std::shared_ptr<Type> ref_;

  // FUNC
  std::vector<std::shared_ptr<Type>> args_;
  std::shared_ptr<Type> ret_;

  // ARRAY & PTR
  std::shared_ptr<Type> elem_;
  // ARRAY
  int size_;
};

std::shared_ptr<Type> Underlying(std::shared_ptr<Type>);

}  // namespace felis

#endif  // FELIS_CHECK_TYPE_H_
