#ifndef FELIS_NODE_HIR_H_
#define FELIS_NODE_HIR_H_

#include "check/decl.h"
#include "check/type.h"
#include "node/ast.h"
#include "node/node.h"
#include "syntax/rune.h"

namespace felis {

namespace hir {

struct Stmt : Node {
  enum Kind { EXPR, RET, VAR_DECL, ASSIGN, IF, BLOCK };
  virtual Kind StmtKind() const = 0;
  virtual bool IsTerminating() const { return false; }

  bool IsRet() const { return StmtKind() == Kind::RET; }
};

struct Expr : public Stmt {
  Loc begin;
  Loc end;

  Expr(Loc begin, Loc end) : begin(begin), end(end) {}

  enum Kind { BINARY, VALUE, CALL, UNARY };

  virtual Expr::Kind ExprKind() const = 0;
  ;

  virtual std::shared_ptr<Type> Ty() const = 0;

  virtual bool IsConstant() const = 0;

  void Debug();

  Stmt::Kind StmtKind() const override { return Stmt::Kind::EXPR; }

  Loc Begin() const override { return begin; }

  Loc End() const override { return end; }
};

struct Value : public Expr {
  Value(Loc begin, Loc end) : Expr(begin, end) {}

  enum Kind { CONSTANT, VARIABLE };

  virtual Value::Kind ValueKind() const = 0;
  ;

  Expr::Kind ExprKind() const override { return Expr::Kind::VALUE; }

  bool IsConstant() const override { return ValueKind() == Kind::CONSTANT; }
};

struct Constant : public Value {
  Constant(Loc begin, Loc end) : Value(begin, end) {}

  enum Kind { INT, FLOAT, CHAR, BOOL, STRING };

  virtual Constant::Kind ConstantKind() const = 0;
  ;

  Value::Kind ValueKind() const override { return Value::Kind::CONSTANT; }
};

struct IntConstant : public Constant {
  int64_t val;
  bool is32;

  IntConstant(Loc begin, Loc end, int64_t val)
      : Constant(begin, end), val(val), is32(val <= INT32_MAX){};

  std::shared_ptr<Type> Ty() const override {
    return std::make_shared<Type>(is32 ? Type::Kind::I32 : Type::Kind::I64);
  }

  Constant::Kind ConstantKind() const override { return Constant::Kind::INT; };
};

struct FloatConstant : public Constant {
  Loc begin;
  Loc end;
  double val;
  bool is32;

  FloatConstant(Loc begin, Loc end, double val, bool is32)
      : Constant(begin, end), end(end), val(val), is32(is32){};

  std::shared_ptr<Type> Ty() const override {
    return std::make_shared<Type>(is32 ? Type::Kind::F32 : Type::Kind::F64);
  }

  Constant::Kind ConstantKind() const override {
    return Constant::Kind::FLOAT;
  };
};

struct CharConstant : public Constant {
  rune val;

  CharConstant(Loc begin, Loc end, rune val) : Constant(begin, end), val(val){};

  std::shared_ptr<Type> Ty() const override {
    return std::make_shared<Type>(Type::Kind::CHAR);
  }

  Constant::Kind ConstantKind() const override { return Constant::Kind::CHAR; };
};

struct BoolConstant : public Constant {
  BoolConstant(Loc begin, Loc end, bool val) : Constant(begin, end), val(val){};

  bool val;

  std::shared_ptr<Type> Ty() const override {
    return std::make_shared<Type>(Type::Kind::BOOL);
  }

  Constant::Kind ConstantKind() const override { return Constant::Kind::BOOL; };
};

struct StringConstant : public Constant {
  StringConstant(Loc begin, Loc end, std::string val)
      : Constant(begin, end), val(val){};

  std::string val;

  std::shared_ptr<Type> Ty() const override {
    return std::make_shared<Type>(Type::Kind::STRING);
  }

  Constant::Kind ConstantKind() const override {
    return Constant::Kind::STRING;
  };
};

struct Call : public Expr {
  std::shared_ptr<Decl> decl;
  unique_deque<Expr> args;

  Call(Loc begin, Loc end) : Expr(begin, end) {}

  std::shared_ptr<Type> Ty() const override {
    auto fnType = (FuncType*)decl->type.get();
    return fnType->ret;
  }

  Expr::Kind ExprKind() const override { return Expr::Kind::CALL; }

  bool IsConstant() const override { return false; }
};

struct Variable : public Value {
  std::shared_ptr<Decl> decl;

  Variable(Loc begin, Loc end) : Value(begin, end){};

  std::shared_ptr<Type> Ty() const override { return decl->type; }

  Value::Kind ValueKind() const override { return Value::Kind::VARIABLE; }
};

struct Unary : public Expr {
  enum Op { NEG, NOT };

  Unary::Op op;
  std::unique_ptr<Expr> expr;

  Unary(Loc begin, Loc end, Unary::Op op, std::unique_ptr<Expr> expr)
      : Expr(begin, end), op(op), expr(std::move(expr)) {}

  std::shared_ptr<Type> Ty() const override { return expr->Ty(); }

  Expr::Kind ExprKind() const override { return Expr::Kind::UNARY; }

  bool IsConstant() const override { return false; }
};

struct Binary : public Expr {
  enum Op {
    LT,
    LE,
    GT,
    GE,
    ADD,
    SUB,

    MUL,
    DIV,
    MOD
  };

  Binary::Op op;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;

  Binary(Loc begin, Loc end, Binary::Op op, std::unique_ptr<Expr> lhs,
         std::unique_ptr<Expr> rhs)
      : Expr(begin, end), op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  std::shared_ptr<Type> Ty() const override {
    switch (op) {
      case Binary::Op::GE:
      case Binary::Op::GT:
      case Binary::Op::LE:
      case Binary::Op::LT:
        return std::make_shared<Type>(Type::Kind::BOOL);
      default:
        // TODO
        return lhs->Ty();
    }
  }

  Expr::Kind ExprKind() const override { return Expr::Kind::BINARY; }

  bool IsConstant() const override { return false; }
};

struct Block : public Stmt {
  Loc begin;
  Loc end;
  unique_deque<Stmt> stmts;

  Block(Loc begin, Loc end, unique_deque<Stmt> stmts)
      : begin(begin), end(end), stmts(std::move(stmts)) {}

  Kind StmtKind() const override { return Stmt::Kind::BLOCK; }

  bool IsTerminating() const override {
    if (stmts.empty()) return false;
    return stmts.back()->IsTerminating();
  }

  Loc Begin() const override { return begin; }

  Loc End() const override { return end; }
};

struct RetStmt : public Stmt {
  Loc begin;
  std::unique_ptr<Expr> expr;

  RetStmt(Loc begin, std::unique_ptr<Expr> expr = nullptr)
      : expr(std::move(expr)) {}

  bool IsTerminating() const override { return true; }

  Kind StmtKind() const override { return Stmt::Kind::RET; }

  Loc Begin() const override { return begin; }

  Loc End() const override {
    if (expr)
      return expr->End();
    else
      return begin + 2;
  }
};

struct VarDeclStmt : public Stmt {
  Loc begin;
  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  VarDeclStmt(Loc begin, std::shared_ptr<Decl> decl, std::unique_ptr<Expr> expr)
      : begin(begin), decl(decl), expr(std::move(expr)) {}

  Kind StmtKind() const override { return Stmt::Kind::VAR_DECL; }

  Loc Begin() const override { return begin; }

  Loc End() const override { return expr->End(); }
};

struct AssignStmt : public Stmt {
  Loc begin;
  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  AssignStmt(Loc begin, std::shared_ptr<Decl> decl, std::unique_ptr<Expr> expr)
      : begin(begin), decl(decl), expr(std::move(expr)) {}

  Kind StmtKind() const override { return Stmt::Kind::ASSIGN; }

  Loc Begin() const override { return begin; }

  Loc End() const override { return expr->End(); }
};

struct IfStmt : public Stmt {
  Loc begin;
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Stmt> els;  // null or Block or IfStmt

  IfStmt(Loc begin, std::unique_ptr<Expr> cond, std::unique_ptr<Block> block,
         std::unique_ptr<Stmt> els = nullptr)
      : begin(begin),
        cond(std::move(cond)),
        block(std::move(block)),
        els(std::move(els)) {}

  Kind StmtKind() const override { return Stmt::Kind::IF; }

  bool IsTerminating() const override {
    if (els == nullptr) return false;
    return block->IsTerminating() && els->IsTerminating();
  }

  Loc Begin() const override { return begin; }

  Loc End() const override {
    if (els)
      return els->End();
    else
      return block->End();
  }
};

struct FnDecl : public Node {
  Loc begin;
  std::shared_ptr<Decl> decl;
  std::deque<std::shared_ptr<Decl>> args;
  std::unique_ptr<Block> block;

  FnDecl(Loc begin, std::shared_ptr<Decl> decl) : begin(begin), decl(decl) {}

  Loc Begin() const override { return begin; }

  Loc End() const override { return block->End(); }
};

struct Extern : public Node {
  Loc begin;
  Loc end;
  std::shared_ptr<Decl> decl;

  Extern(Loc begin, Loc end, std::shared_ptr<Decl> decl) : decl(decl) {}

  Loc Begin() const override { return begin; }

  Loc End() const override { return end; }
};

struct File {
  std::vector<std::unique_ptr<Extern>> externs;
  unique_deque<FnDecl> fnDecls;
};

}  // namespace hir

}  // namespace felis

#endif  // FELIS_NODE_HIR_H_
