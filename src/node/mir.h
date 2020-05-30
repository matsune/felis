#ifndef FELIS_NODE_MIR_H_
#define FELIS_NODE_MIR_H_

#include "check/type.h"
#include "node/ast.h"

namespace felis {

namespace mir {

struct Func;
struct Function;
struct BB;

struct Value {
  enum Kind {
    CONST_INT,
    CONST_FLOAT,
    CONST_STRING,
    CONST_BOOL,
    ALLOC,
    VAL,
  };

  virtual Value::Kind ValueKind() const = 0;
};

struct Constant : Value {
  std::shared_ptr<Type> type;

  Constant(std::shared_ptr<Type> type) : type(type){};
};

struct ConstantInt : Constant {
  int64_t val;

  ConstantInt(std::shared_ptr<Type> type, int64_t val)
      : Constant(type), val(val){};

  Value::Kind ValueKind() const override { return Value::Kind::CONST_INT; }
};

struct ConstantFloat : Constant {
  double val;

  ConstantFloat(std::shared_ptr<Type> type, double val)
      : Constant(type), val(val){};

  Value::Kind ValueKind() const override { return Value::Kind::CONST_FLOAT; }
};

struct ConstantString : Constant {
  std::string val;

  ConstantString(std::shared_ptr<Type> type, std::string val)
      : Constant(type), val(val){};

  Value::Kind ValueKind() const override { return Value::Kind::CONST_STRING; }
};

struct ConstantBool : Constant {
  bool val;

  ConstantBool(std::shared_ptr<Type> type, bool val)
      : Constant(type), val(val){};

  Value::Kind ValueKind() const override { return Value::Kind::CONST_BOOL; }
};

struct LValue : Value {
  /* bool IsAlloc() const { return ValueKind() == Value::Kind::ALLOC; } */
};

using LValueID = uint;

struct Alloc : LValue {
  LValueID id;
  std::shared_ptr<Type> type;

  Alloc(LValueID id, std::shared_ptr<Type> type) : id(id), type(type) {}

  Value::Kind ValueKind() const override { return Value::Kind::ALLOC; }
};

struct Val : LValue {
  LValueID id;
  std::shared_ptr<Type> type;

  Val(LValueID id, std::shared_ptr<Type> type) : id(id), type(type) {}

  Value::Kind ValueKind() const override { return Value::Kind::VAL; }
};

struct Inst {
  enum Kind { STORE, LOAD, UNARY, COMP, BINARY, CALL, RET, ARRAY, COND };

  virtual Inst::Kind InstKind() = 0;
};

struct Store : Inst {
  std::shared_ptr<Alloc> alloc;
  std::shared_ptr<Value> val;

  Store(std::shared_ptr<Alloc> alloc, std::shared_ptr<Value> val)
      : alloc(alloc), val(val) {}

  Inst::Kind InstKind() override { return Inst::Kind::STORE; }
};

struct Load : Inst {
  std::shared_ptr<Val> val;
  std::shared_ptr<Alloc> alloc;

  Load(std::shared_ptr<Val> val, std::shared_ptr<Alloc> alloc)
      : val(val), alloc(alloc) {}

  Inst::Kind InstKind() override { return Inst::Kind::LOAD; }
};

struct Call : Inst {
  std::shared_ptr<Val> val;
  std::vector<std::shared_ptr<Value>> args;
  std::shared_ptr<Func> func;

  Call(std::shared_ptr<Val> val, std::vector<std::shared_ptr<Value>> args,
       std::shared_ptr<Func> func)
      : val(val), args(args), func(func) {}

  Inst::Kind InstKind() override { return Inst::Kind::CALL; }
};

struct Ret : Inst {
  std::shared_ptr<Value> val;

  Ret(std::shared_ptr<Value> val) : val(val) {}

  Inst::Kind InstKind() override { return Inst::Kind::RET; }
};

struct Unary : Inst {
  enum Op { NEG, NOT };
  std::shared_ptr<Val> val;
  std::shared_ptr<Value> value;
  Op op;

  Unary(std::shared_ptr<Val> val, std::shared_ptr<Value> value, Op op)
      : val(val), value(value), op(op) {}

  Inst::Kind InstKind() override { return Inst::Kind::UNARY; }
};

struct Comp : Inst {
  enum Op {
    EQEQ,
    NEQ,
    LT,
    LE,
    GT,
    GE,
  };
  std::shared_ptr<Val> val;
  std::shared_ptr<Value> lhs;
  std::shared_ptr<Value> rhs;
  Op op;

  Comp(std::shared_ptr<Val> val, std::shared_ptr<Value> lhs,
       std::shared_ptr<Value> rhs, Op op)
      : val(val), lhs(lhs), rhs(rhs), op(op) {}

  Inst::Kind InstKind() override { return Inst::Kind::COMP; }
};

struct Binary : Inst {
  enum Op { ADD, SUB, MUL, DIV, MOD };
  std::shared_ptr<Val> val;
  std::shared_ptr<Value> lhs;
  std::shared_ptr<Value> rhs;
  Op op;

  Binary(std::shared_ptr<Val> val, std::shared_ptr<Value> lhs,
         std::shared_ptr<Value> rhs, Op op)
      : val(val), lhs(lhs), rhs(rhs), op(op) {}

  Inst::Kind InstKind() override { return Inst::Kind::BINARY; }
};

struct Array : Inst {
  std::shared_ptr<Val> val;
  std::vector<std::shared_ptr<Value>> values;

  Array(std::shared_ptr<Val> val, std::vector<std::shared_ptr<Value>> values)
      : val(val), values(values) {}

  Inst::Kind InstKind() override { return Inst::Kind::ARRAY; }
};

struct Cond : Inst {
  std::shared_ptr<Value> cond;
  std::shared_ptr<BB> then_bb;
  std::shared_ptr<BB> otherwise_bb;

  Cond(std::shared_ptr<Value> cond, std::shared_ptr<BB> then_bb,
       std::shared_ptr<BB> otherwise_bb)
      : cond(cond), then_bb(then_bb), otherwise_bb(otherwise_bb) {}

  Inst::Kind InstKind() override { return Inst::Kind::COND; }
};

struct BB {
  /* int id; */
  std::vector<std::shared_ptr<Inst>> instructions;
  std::shared_ptr<BB> next_bb;
  Function& parent;

  BB(Function& parent) : parent(parent) {}
};

using FunctionID = int;

struct Func {
  FunctionID id;
  std::string name;
  std::shared_ptr<FuncType> type;

  Func(FunctionID id, std::string name, std::shared_ptr<FuncType> type)
      : id(id), name(name), type(type) {}

  virtual bool IsExt() { return true; }
};

struct Function : Func {
  std::vector<std::shared_ptr<Alloc>> args;
  std::vector<std::shared_ptr<LValue>> vals;
  std::shared_ptr<BB> entry_bb;

  Function(FunctionID id, std::string name, std::shared_ptr<FuncType> type)
      : Func(id, name, type), entry_bb(new BB(*this)) {}

  bool IsExt() override { return false; }
};

struct File {
  std::vector<std::shared_ptr<Func>> funcs;
};

}  // namespace mir

}  // namespace felis

#endif  // FELIS_NODE_MIR_H_
