#ifndef FELIS_NODE_MIR_H_
#define FELIS_NODE_MIR_H_

#include "check/type.h"
#include "node/ast.h"

namespace felis {

namespace mir {

struct Func;
struct Function;
struct BB;

struct RValue {
  enum Kind {
    CONST_INT,
    CONST_FLOAT,
    CONST_STRING,
    CONST_BOOL,
    VAL,
  };

  virtual RValue::Kind RValueKind() const = 0;

  std::shared_ptr<FixedType> type;

  RValue(std::shared_ptr<FixedType> type) : type(type) {}
};

struct Constant : RValue {
  Constant(std::shared_ptr<FixedType> type) : RValue(type){};
};

struct ConstantInt : Constant {
  int64_t val;

  ConstantInt(std::shared_ptr<FixedType> type, int64_t val)
      : Constant(type), val(val){};

  RValue::Kind RValueKind() const override { return RValue::Kind::CONST_INT; }
};

struct ConstantFloat : Constant {
  double val;

  ConstantFloat(std::shared_ptr<FixedType> type, double val)
      : Constant(type), val(val){};

  RValue::Kind RValueKind() const override { return RValue::Kind::CONST_FLOAT; }
};

struct ConstantString : Constant {
  std::string val;

  ConstantString(std::string val) : Constant(kTypeString), val(val){};

  RValue::Kind RValueKind() const override {
    return RValue::Kind::CONST_STRING;
  }
};

struct ConstantBool : Constant {
  bool val;

  ConstantBool(bool val) : Constant(kTypeBool), val(val){};

  RValue::Kind RValueKind() const override { return RValue::Kind::CONST_BOOL; }
};

struct Local {
  using ID = int;
  ID id;

  Local(ID id) : id(id) {}
};

struct Val : RValue, Local {
  Val(ID id, std::shared_ptr<FixedType> type) : RValue(type), Local(id){};

  RValue::Kind RValueKind() const override { return RValue::Kind::VAL; }
};

struct LValue : Local {
  std::string name;
  std::shared_ptr<PtrType> type;

  LValue(ID id, std::shared_ptr<PtrType> type, std::string name = "")
      : Local(id), type(type), name(name){};
};

struct Inst {
  enum Kind {
    ALLOC,
    LOAD,
    STORE,
    UNARY,
    BINARY,
    CMP,
    ARRAY,
    CALL,
    BR,
    GOTO,
    RET
  };

  virtual Inst::Kind InstKind() const = 0;
};

// lval: *T = alloc T
struct AllocInst : Inst {
  std::shared_ptr<LValue> lval;

  AllocInst(std::shared_ptr<LValue> lval) : lval(lval) {}

  Inst::Kind InstKind() const override { return Inst::Kind::ALLOC; }
};

// rval: T = load lval: *T
struct LoadInst : Inst {
  std::shared_ptr<RValue> rval;
  std::shared_ptr<LValue> lval;

  LoadInst(std::shared_ptr<RValue> rval, std::shared_ptr<LValue> lval)
      : rval(rval), lval(lval) {}

  Inst::Kind InstKind() const override { return Inst::Kind::LOAD; }
};

// lval: *T = store rval: T
struct StoreInst : Inst {
  std::shared_ptr<LValue> lval;
  std::shared_ptr<RValue> rval;

  StoreInst(std::shared_ptr<LValue> lval, std::shared_ptr<RValue> rval)
      : lval(lval), rval(rval) {}

  Inst::Kind InstKind() const override { return Inst::Kind::STORE; }
};

// val: T = Op(operand: T)
struct UnaryInst : Inst {
  enum Op { NEG, NOT };
  std::shared_ptr<Val> val;
  Op op;
  std::shared_ptr<RValue> operand;

  UnaryInst(std::shared_ptr<Val> val, Op op, std::shared_ptr<RValue> operand)
      : val(val), op(op), operand(operand) {
    assert(*val->type == *operand->type);
  }

  Inst::Kind InstKind() const override { return Inst::Kind::UNARY; }
};

// val: T = Op(lhs: T, rhs: T)
struct BinaryInst : Inst {
  enum Op { ADD, SUB, MUL, DIV, MOD };
  std::shared_ptr<Val> val;
  Op op;
  std::shared_ptr<RValue> lhs;
  std::shared_ptr<RValue> rhs;

  BinaryInst(std::shared_ptr<Val> val, Op op, std::shared_ptr<RValue> lhs,
             std::shared_ptr<RValue> rhs)
      : val(val), op(op), lhs(lhs), rhs(rhs) {
    assert(*val->type == *lhs->type);
    assert(*val->type == *rhs->type);
  }

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
  std::shared_ptr<Val> val;
  Op op;
  std::shared_ptr<RValue> lhs;
  std::shared_ptr<RValue> rhs;

  CmpInst(std::shared_ptr<Val> val, Op op, std::shared_ptr<RValue> lhs,
          std::shared_ptr<RValue> rhs)
      : val(val), op(op), lhs(lhs), rhs(rhs) {
    assert(*val->type == *kTypeBool);
  }

  Inst::Kind InstKind() const override { return Inst::Kind::CMP; }
};

// val: [T, n] = [value: T, ...]
struct ArrayInst : Inst {
  std::shared_ptr<Val> val;
  std::vector<std::shared_ptr<RValue>> values;

  ArrayInst(std::shared_ptr<Val> val,
            std::vector<std::shared_ptr<RValue>> values)
      : val(val), values(values) {}

  Inst::Kind InstKind() const override { return Inst::Kind::ARRAY; }
};

// val: T* = gep lval: [T], idx: int
/* struct GepInst : Inst { */
/*   std::shared_ptr<Val> val; */
/* }; */

// val: T = call func(arg: S, ...)
struct CallInst : Inst {
  std::shared_ptr<Val> val;
  std::vector<std::shared_ptr<RValue>> args;
  std::shared_ptr<Func> func;

  CallInst(std::shared_ptr<Val> val, std::vector<std::shared_ptr<RValue>> args,
           std::shared_ptr<Func> func)
      : val(val), args(args), func(func) {}

  Inst::Kind InstKind() const override { return Inst::Kind::CALL; }
};

// br(cond: bool) [then: then_bb, otherwise: else_bb]
struct BrInst : Inst {
  std::shared_ptr<RValue> cond;
  std::shared_ptr<BB> then_bb;
  std::shared_ptr<BB> else_bb;

  BrInst(std::shared_ptr<RValue> cond, std::shared_ptr<BB> then_bb,
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
  std::shared_ptr<RValue> val;

  RetInst(std::shared_ptr<RValue> val) : val(val) {}

  Inst::Kind InstKind() const override { return Inst::Kind::RET; }
};

// BasicBlock
// Each of BB nodes have a reference to a next BB node.
struct BB {
  using ID = int;

  ID id;
  std::vector<std::shared_ptr<Inst>> instructions;
  std::shared_ptr<BB> next_bb;
  Function& parent;

  BB(ID id, Function& parent) : id(id), parent(parent) {}

  void InsertInst(std::shared_ptr<Inst> inst) { instructions.push_back(inst); }
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
  std::vector<std::shared_ptr<RValue>> args;
  std::shared_ptr<BB> entry_bb;

  Function(FunctionID id, std::string name, std::shared_ptr<FuncType> type)
      : Func(id, name, type),
        next_local_id(0),
        next_bb_id(1),
        entry_bb(new BB(0, *this)) {}

  bool IsExt() override { return false; }

  mir::Local::ID GenLocalID() { return next_local_id++; }
  mir::BB::ID GenBBID() { return next_bb_id++; }

 private:
  mir::Local::ID next_local_id;
  mir::BB::ID next_bb_id;
};

struct File {
  std::vector<std::shared_ptr<Func>> funcs;
};

}  // namespace mir

}  // namespace felis

#endif  // FELIS_NODE_MIR_H_
