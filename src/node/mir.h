#ifndef FELIS_NODE_MIR_H_
#define FELIS_NODE_MIR_H_

#include <map>
#include <utility>

#include "check/decl.h"
#include "check/type.h"
#include "node/ast.h"

namespace felis {

namespace mir {

struct Func;
struct Function;
struct BB;

struct Value {
  enum Kind { CONST_INT, CONST_FLOAT, CONST_BOOL, CONST_STRING, RESULT, INDEX };
  virtual Value::Kind ValueKind() const = 0;

  std::shared_ptr<Type> type;

  Value(std::shared_ptr<Type> type) : type(type) {}

  bool IsConstInt() const { return ValueKind() == Value::Kind::CONST_INT; }
  bool IsConstFloat() const { return ValueKind() == Value::Kind::CONST_FLOAT; }
  bool IsConstBool() const { return ValueKind() == Value::Kind::CONST_BOOL; }
  bool IsConstString() const {
    return ValueKind() == Value::Kind::CONST_STRING;
  }
  bool IsResult() const { return ValueKind() == Value::Kind::RESULT; }
  bool IsIndex() const { return ValueKind() == Value::Kind::INDEX; }
};

struct ConstInt : Value {
  int64_t val;

  ConstInt(std::shared_ptr<Type> type, int64_t val) : Value(type), val(val) {}

  Value::Kind ValueKind() const override { return Value::Kind::CONST_INT; }
};

struct ConstFloat : Value {
  double val;

  ConstFloat(std::shared_ptr<Type> type, double val) : Value(type), val(val) {}

  Value::Kind ValueKind() const override { return Value::Kind::CONST_FLOAT; }
};

struct ConstBool : Value {
  bool val;

  ConstBool(bool val) : Value(Type::MakeBool()), val(val) {}

  Value::Kind ValueKind() const override { return Value::Kind::CONST_BOOL; }
};

struct ConstString : Value {
  std::string val;

  ConstString(std::string val) : Value(Type::MakeString()), val(val) {}

  Value::Kind ValueKind() const override { return Value::Kind::CONST_STRING; }
};

struct Result : Value {
  using ID = uint;
  ID id;

  Result(std::shared_ptr<Type> ty, ID id) : Value(ty), id(id) {}

  Value::Kind ValueKind() const override { return Value::Kind::RESULT; }
};

// $1[$2]
struct Index : Value {
  std::shared_ptr<Value> val;
  std::shared_ptr<Value> idx;

  Index(std::shared_ptr<Type> ty, std::shared_ptr<Value> val,
        std::shared_ptr<Value> idx)
      : Value(ty), val(val), idx(idx) {}

  Value::Kind ValueKind() const override { return Value::Kind::INDEX; }
};

struct Inst {
  enum Kind { ASSIGN, UNARY, BINARY, CMP, ARRAY, CALL, BR, GOTO, RET, PHI };

  virtual Inst::Kind InstKind() const = 0;
};

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
  std::shared_ptr<Result> result;
  Op op;
  std::shared_ptr<Value> operand;

  UnaryInst(std::shared_ptr<Result> result, Op op,
            std::shared_ptr<Value> operand)
      : result(result), op(op), operand(operand) {}

  Inst::Kind InstKind() const override { return Inst::Kind::UNARY; }
};

// var: T = Op(lhs: T, rhs: T)
struct BinaryInst : Inst {
  enum Op { ADD, SUB, MUL, DIV, MOD };
  std::shared_ptr<Result> result;
  Op op;
  std::shared_ptr<Value> lhs;
  std::shared_ptr<Value> rhs;

  BinaryInst(std::shared_ptr<Result> result, Op op, std::shared_ptr<Value> lhs,
             std::shared_ptr<Value> rhs)
      : result(result), op(op), lhs(lhs), rhs(rhs) {}

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
  std::shared_ptr<Result> result;
  Op op;
  std::shared_ptr<Value> lhs;
  std::shared_ptr<Value> rhs;

  CmpInst(std::shared_ptr<Result> result, Op op, std::shared_ptr<Value> lhs,
          std::shared_ptr<Value> rhs)
      : result(result), op(op), lhs(lhs), rhs(rhs) {}

  Inst::Kind InstKind() const override { return Inst::Kind::CMP; }
};

// $0 = [$1, $2...]
struct ArrayInst : Inst {
  std::shared_ptr<Result> result;
  std::vector<std::shared_ptr<Value>> values;

  ArrayInst(std::shared_ptr<Result> result,
            std::vector<std::shared_ptr<Value>> values)
      : result(result), values(values) {}

  Inst::Kind InstKind() const override { return Inst::Kind::ARRAY; }
};

// var: T = call func(args...) -> T
struct CallInst : Inst {
  std::shared_ptr<Result> result;
  std::vector<std::shared_ptr<Value>> args;
  std::shared_ptr<Func> func;

  CallInst(std::shared_ptr<Result> result,
           std::vector<std::shared_ptr<Value>> args, std::shared_ptr<Func> func)
      : result(result), args(args), func(func) {}

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
    assert(cond->type->IsBool());
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

// val = phi [val1, BB], [val2, BB]
struct PhiInst : Inst {
  std::shared_ptr<Result> result;
  std::vector<std::pair<std::shared_ptr<Value>, std::shared_ptr<BB>>> nodes;

  PhiInst(std::shared_ptr<Result> result) : result(result) {}

  Inst::Kind InstKind() const override { return Inst::Kind::PHI; }
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
  std::shared_ptr<Type> type;

  Func(std::string name, std::shared_ptr<Type> type) : name(name), type(type) {
    assert(type->IsFunc());
  }

  virtual bool IsExt() { return true; }
};

struct Function : Func {
  std::vector<std::shared_ptr<Value>> args;
  std::shared_ptr<BB> entry_bb;

  std::map<std::shared_ptr<Decl>, std::shared_ptr<mir::Value>> decl_value_map;
  std::vector<std::shared_ptr<mir::Result>> alloc_list;

  Function(std::string name, std::shared_ptr<Type> type)
      : Func(name, type),
        next_result_id(1),
        next_bb_id(1),
        entry_bb(new BB(0, *this)) {
    assert(type->IsFunc());
  }

  bool IsExt() override { return false; }

  void InsertAlloc(std::shared_ptr<mir::Result> result) {
    alloc_list.push_back(result);
  }

  mir::Result::ID GenResultID() { return next_result_id++; }
  mir::BB::ID GenBBID() { return next_bb_id++; }

 private:
  mir::Result::ID next_result_id;
  mir::BB::ID next_bb_id;
};

struct File {
  std::vector<std::shared_ptr<Func>> funcs;

  std::map<std::shared_ptr<Decl>, std::shared_ptr<mir::Func>> decl_fn_map;
};

}  // namespace mir

}  // namespace felis

#endif  // FELIS_NODE_MIR_H_
