#ifndef FELIS_CHECK_HIR_H_
#define FELIS_CHECK_HIR_H_

#include "check/decl.h"
#include "check/type.h"
#include "syntax/ast.h"

namespace felis {

namespace hir {

struct Expr {
  enum Kind { BINARY, VALUE, CALL, UNARY };
  virtual Kind ExprKind() = 0;
  virtual std::shared_ptr<Type> Ty() = 0;
  Expr(Pos pos) : pos(pos){};
  Pos pos;

  void Debug();
};

struct Value : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::VALUE; }

  enum Kind { CONSTANT, VARIABLE };

  virtual Kind ValueKind() = 0;
  Value(Pos pos) : Expr(pos){};
};

struct Constant : public Value {
  Value::Kind ValueKind() { return Value::Kind::CONSTANT; }

  enum Kind { INT, FLOAT, CHAR, BOOL, STRING };

  virtual Constant::Kind ConstantKind() = 0;
  Constant(Pos pos) : Value(pos){};
};

struct IntConstant : public Constant {
  IntConstant(Pos pos, uint64_t val, bool is32 = false)
      : Constant(pos), val(val), is32(is32){};
  uint64_t val;
  bool is32;

  Constant::Kind ConstantKind() { return Constant::Kind::INT; };

  std::shared_ptr<Type> Ty() {
    // TODO: I64
    return std::make_shared<Type>(Type::Kind::I32);
  }
};

struct FloatConstant : public Constant {
  FloatConstant(Pos pos, double val, bool is32 = false)
      : Constant(pos), val(val), is32(is32){};
  double val;
  bool is32;

  Constant::Kind ConstantKind() { return Constant::Kind::FLOAT; };

  std::shared_ptr<Type> Ty() {
    // TODO: F64
    return std::make_shared<Type>(Type::Kind::F32);
  }
};

struct CharConstant : public Constant {
  CharConstant(Pos pos, rune val) : Constant(pos), val(val){};
  rune val;

  Constant::Kind ConstantKind() { return Constant::Kind::CHAR; };

  std::shared_ptr<Type> Ty() {
    return std::make_shared<Type>(Type::Kind::CHAR);
  }
};

struct BoolConstant : public Constant {
  BoolConstant(Pos pos, bool val) : Constant(pos), val(val){};
  bool val;

  Constant::Kind ConstantKind() { return Constant::Kind::BOOL; };

  std::shared_ptr<Type> Ty() {
    return std::make_shared<Type>(Type::Kind::BOOL);
  }
};

struct StringConstant : public Constant {
  StringConstant(Pos pos, std::string val) : Constant(pos), val(val){};
  std::string val;

  Constant::Kind ConstantKind() { return Constant::Kind::STRING; };

  std::shared_ptr<Type> Ty() {
    return std::make_shared<Type>(Type::Kind::STRING);
  }
};

struct Call : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::CALL; }

  Call(Pos pos) : Expr(pos) {}
  std::shared_ptr<Decl> decl;
  std::vector<std::unique_ptr<Expr>> argExprs;

  std::shared_ptr<Type> Ty() {
    auto fnType = (FuncType*)decl->type.get();
    return fnType->ret;
  }
};

struct Variable : public Value {
  Value::Kind ValueKind() { return Value::Kind::VARIABLE; }
  Variable(Pos pos) : Value(pos){};
  std::shared_ptr<Decl> decl;

  std::shared_ptr<Type> Ty() { return decl->type; }
};

struct Unary : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::UNARY; }

  Unary(Pos pos, UnOp unOp, std::unique_ptr<Expr> exp)
      : Expr(pos), unOp(unOp), exp(std::move(exp)) {}

  UnOp unOp;
  std::unique_ptr<Expr> exp;

  std::shared_ptr<Type> Ty() { return exp->Ty(); }
};

struct Binary : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::BINARY; }

  Binary(Pos pos, BinOp binOp, std::unique_ptr<Expr> lhs,
         std::unique_ptr<Expr> rhs)
      : Expr(pos), binOp(binOp), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  BinOp binOp;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  std::shared_ptr<Type> Ty() {
    switch (binOp) {
      case BinOp::GE:
      case BinOp::GT:
      case BinOp::LE:
      case BinOp::LT:
        return std::make_shared<Type>(Type::Kind::BOOL);
      default:
        // TODO
        return lhs->Ty();
    }
  }
};

}  // namespace hir

}  // namespace felis

#endif  // FELIS_CHECK_HIR_H_
