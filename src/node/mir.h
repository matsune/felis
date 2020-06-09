#ifndef FELIS_NODE_MIR_H_
#define FELIS_NODE_MIR_H_

#include <map>

#include "check/decl.h"
#include "check/ty.h"
#include "node/ast.h"

namespace felis {

namespace mir {

struct Func;
struct Function;
struct BB;

struct Value {
  using ID = int;
  ID id;
  std::shared_ptr<Ty> type;

  Value(std::shared_ptr<Ty> type, ID id = 0) : type(type), id(id) {}

  virtual bool IsRValue() const { return false; }
  virtual bool IsConst() const { return false; }
  virtual bool IsConstInt() const { return false; }
  virtual bool IsConstFloat() const { return false; }
  virtual bool IsConstBool() const { return false; }

  virtual bool IsLValue() const { return false; }
  virtual bool IsConstString() const { return false; }
};

struct RValue : Value {
  RValue(std::shared_ptr<Ty> type, Value::ID id = 0) : Value(type, id) {}

  bool IsRValue() const override { return true; }
};

struct ConstInt : RValue {
  int64_t val;

  ConstInt(std::shared_ptr<Ty> type, int64_t val) : RValue(type), val(val) {}

  bool IsConst() const override { return true; }
  bool IsConstInt() const override { return true; }
};

struct ConstFloat : RValue {
  double val;

  ConstFloat(std::shared_ptr<Ty> type, double val) : RValue(type), val(val) {}

  bool IsConst() const override { return true; }
  bool IsConstFloat() const override { return true; }
};

struct ConstBool : RValue {
  bool val;

  ConstBool(bool val) : RValue(kTypeBool), val(val) {}

  bool IsConst() const override { return true; }
  bool IsConstBool() const override { return true; }
};

struct LValue : Value {
  LValue(std::shared_ptr<Ty> type, Value::ID id = 0) : Value(type, id) {
    assert(type->IsPtr() || type->IsString());
  }

  bool IsLValue() const override { return true; }
};

struct ConstString : LValue {
  std::string val;

  ConstString(std::string val) : LValue(kTypeString, 0), val(val) {}

  bool IsConstString() const override { return true; }
};

struct Inst {
  enum Kind { ASSIGN, UNARY, BINARY, CMP, ARRAY, CALL, BR, GOTO, RET };

  virtual Inst::Kind InstKind() const = 0;
};

// into: *T <= value: T
//
// Assign is not only for llvm store instruction, also for memcpy.
struct AssignInst : Inst {
  std::shared_ptr<Value> into;
  std::shared_ptr<Value> value;

  AssignInst(std::shared_ptr<Value> into, std::shared_ptr<Value> value)
      : into(into), value(value) {}

  Inst::Kind InstKind() const override { return Inst::Kind::ASSIGN; }
};

// var: T = Op(operand: T)
struct UnaryInst : Inst {
  enum Op { NEG, NOT };
  std::shared_ptr<Value> var;
  Op op;
  std::shared_ptr<Value> operand;

  UnaryInst(std::shared_ptr<Value> var, Op op, std::shared_ptr<Value> operand)
      : var(var), op(op), operand(operand) {
    assert(*var->type == *operand->type);
  }

  Inst::Kind InstKind() const override { return Inst::Kind::UNARY; }
};

// var: T = Op(lhs: T, rhs: T)
struct BinaryInst : Inst {
  enum Op { ADD, SUB, MUL, DIV, MOD };
  std::shared_ptr<Value> var;
  Op op;
  std::shared_ptr<Value> lhs;
  std::shared_ptr<Value> rhs;

  BinaryInst(std::shared_ptr<Value> var, Op op, std::shared_ptr<Value> lhs,
             std::shared_ptr<Value> rhs)
      : var(var), op(op), lhs(lhs), rhs(rhs) {}

  Inst::Kind InstKind() const override { return Inst::Kind::BINARY; }
};

// val: bool = Op(lhs: T, rhs: T)
struct CmpInst : Inst {
  enum Op {
    EQEQ,
    NEQ,
    LT,
    LE,
    GT,
    GE,
  };
  std::shared_ptr<Value> var;
  Op op;
  std::shared_ptr<Value> lhs;
  std::shared_ptr<Value> rhs;

  CmpInst(std::shared_ptr<Value> var, Op op, std::shared_ptr<Value> lhs,
          std::shared_ptr<Value> rhs)
      : var(var), op(op), lhs(lhs), rhs(rhs) {
    assert(*var->type == *kTypeBool);
  }

  Inst::Kind InstKind() const override { return Inst::Kind::CMP; }
};

// $0 = [$1, $2...]
struct ArrayInst : Inst {
  std::shared_ptr<Value> var;
  std::vector<std::shared_ptr<Value>> values;

  ArrayInst(std::shared_ptr<Value> var,
            std::vector<std::shared_ptr<Value>> values)
      : var(var), values(values) {}

  Inst::Kind InstKind() const override { return Inst::Kind::ARRAY; }
};

// var: T* = gep arr: [T] at idx
// struct GepInst : Inst {
//  std::shared_ptr<Value> var;
//  std::shared_ptr<Value> arr;
//  int idx;
//
//  GepInst(std::shared_ptr<Value> var, std::shared_ptr<Value> arr, int idx)
//      : var(var), arr(arr), idx(idx) {}
//
//  Inst::Kind InstKind() const override { return Inst::Kind::GEP; }
//};

// var: T = call func(args...) -> T
struct CallInst : Inst {
  std::shared_ptr<Value> var;
  std::vector<std::shared_ptr<Value>> args;
  std::shared_ptr<Func> func;

  CallInst(std::shared_ptr<Value> var, std::vector<std::shared_ptr<Value>> args,
           std::shared_ptr<Func> func)
      : var(var), args(args), func(func) {}

  Inst::Kind InstKind() const override { return Inst::Kind::CALL; }
};

// br(cond: bool) [then: then_bb, otherwise: else_bb]
struct BrInst : Inst {
  std::shared_ptr<Value> cond;
  std::shared_ptr<BB> then_bb;
  std::shared_ptr<BB> else_bb;

  BrInst(std::shared_ptr<Value> cond, std::shared_ptr<BB> then_bb,
         std::shared_ptr<BB> else_bb)
      : cond(cond), then_bb(then_bb), else_bb(else_bb) {
    assert(*cond->type == *kTypeBool);
  }

  Inst::Kind InstKind() const override { return Inst::Kind::BR; }
};

// goto -> goto_bb
struct GotoInst : Inst {
  std::shared_ptr<BB> goto_bb;

  GotoInst(std::shared_ptr<BB> goto_bb) : goto_bb(goto_bb) {}

  Inst::Kind InstKind() const override { return Inst::Kind::GOTO; }
};

// ret val: T?
struct RetInst : Inst {
  std::shared_ptr<Value> val;

  RetInst(std::shared_ptr<Value> val) : val(val) {}

  Inst::Kind InstKind() const override { return Inst::Kind::RET; }
};

// BasicBlock
// Each of BB nodes have a reference to a next BB node.
struct BB {
  using ID = int;

  ID id;
  std::vector<std::shared_ptr<Inst>> instructions;
  std::shared_ptr<BB> next_bb;
  Function &parent;

  BB(ID id, Function &parent) : id(id), parent(parent) {}

  void InsertInst(std::shared_ptr<Inst> inst) { instructions.push_back(inst); }
};

struct Func {
  std::string name;
  std::shared_ptr<FuncTy> type;

  Func(std::string name, std::shared_ptr<FuncTy> type)
      : name(name), type(type) {}

  virtual bool IsExt() { return true; }
};

struct Function : Func {
  std::vector<std::shared_ptr<RValue>> args;
  std::shared_ptr<BB> entry_bb;

  std::map<std::shared_ptr<Decl>, std::shared_ptr<mir::Value>> decl_value_map;
  std::vector<std::shared_ptr<mir::Value>> alloc_list;

  Function(std::string name, std::shared_ptr<FuncTy> type)
      : Func(name, type),
        next_value_id(1),
        next_bb_id(1),
        entry_bb(new BB(0, *this)) {}

  bool IsExt() override { return false; }

  void InsertAllocValue(std::shared_ptr<mir::Value> value) {
    value->id = next_value_id++;
    alloc_list.push_back(value);
  }

  mir::BB::ID GenBBID() { return next_bb_id++; }

 private:
  mir::Value::ID next_value_id;
  mir::BB::ID next_bb_id;
};

struct File {
  std::vector<std::shared_ptr<Func>> funcs;

  std::map<std::shared_ptr<Decl>, std::shared_ptr<mir::Func>> decl_fn_map;
};

}  // namespace mir

}  // namespace felis

#endif  // FELIS_NODE_MIR_H_
