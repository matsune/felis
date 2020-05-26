#ifndef FELIS_NODE_HIR_H_
#define FELIS_NODE_HIR_H_

#include "check/decl.h"
#include "check/type.h"
#include "node/node.h"
#include "syntax/rune.h"

namespace felis {

namespace hir {

struct HirNode : public Node {};

struct Stmt : HirNode {
  enum Kind { EXPR, RET, VAR_DECL, ASSIGN };
  virtual Kind StmtKind() const = 0;
  virtual bool IsTerminating() const { return false; }
};

struct Expr : public Stmt {
  enum Kind { BINARY, VALUE, CALL, UNARY, IF, BLOCK };
  virtual Expr::Kind ExprKind() const = 0;
  virtual std::shared_ptr<FixedType> Type() const = 0;

  // override Stmt
  Stmt::Kind StmtKind() const override { return Stmt::Kind::EXPR; }
};

struct Value : public Expr {
  enum Kind { CONSTANT, VARIABLE };
  virtual Value::Kind ValueKind() const = 0;

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::VALUE; }
};

struct Variable : public Value {
  std::shared_ptr<Decl> decl;

  Variable(std::shared_ptr<Decl> decl) : decl(decl){};

  // override Value
  Value::Kind ValueKind() const override { return Value::Kind::VARIABLE; }

  // override Expr
  std::shared_ptr<FixedType> Type() const override {
    return std::dynamic_pointer_cast<FixedType>(decl->type);
  }
};

struct Constant : public Value {
  enum Kind { INT, FLOAT, BOOL, STRING };
  std::shared_ptr<FixedType> type;

  Constant(std::shared_ptr<FixedType> type) : type(type) {}

  virtual Constant::Kind ConstantKind() = 0;

  // override Value
  Value::Kind ValueKind() const override { return Value::Kind::CONSTANT; }

  // override Expr
  std::shared_ptr<FixedType> Type() const override { return type; }
};

struct IntConstant : public Constant {
  const int64_t val;
  IntConstant(std::shared_ptr<FixedType> type, int64_t val)
      : Constant(type), val(val) {}

  Constant::Kind ConstantKind() override { return Constant::Kind::INT; }
};

struct FloatConstant : public Constant {
  const double val;
  FloatConstant(std::shared_ptr<FixedType> type, double val)
      : Constant(type), val(val) {}

  Constant::Kind ConstantKind() override { return Constant::Kind::FLOAT; }
};

struct BoolConstant : public Constant {
  const bool val;
  BoolConstant(std::shared_ptr<FixedType> type, bool val)
      : Constant(type), val(val) {}

  Constant::Kind ConstantKind() override { return Constant::Kind::BOOL; }
};

struct StringConstant : public Constant {
  std::string val;
  StringConstant(std::shared_ptr<FixedType> type, std::string val)
      : Constant(type), val(val) {}

  Constant::Kind ConstantKind() override { return Constant::Kind::STRING; }
};

struct Call : public Expr {
  std::shared_ptr<Decl> decl;
  unique_deque<Expr> args;

  Call(std::shared_ptr<Decl> decl, unique_deque<Expr> args)
      : Expr(), decl(decl), args(std::move(args)) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::CALL; }

  // override Expr
  std::shared_ptr<FixedType> Type() const override {
    return std::dynamic_pointer_cast<FixedType>(decl->AsFuncType()->ret);
  }
};

struct Unary : public Expr {
  enum Op { NEG, NOT };

  const Unary::Op op;
  std::unique_ptr<Expr> expr;

  Unary(Unary::Op op, std::unique_ptr<Expr> expr)
      : Expr(), op(op), expr(std::move(expr)) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::UNARY; }

  std::shared_ptr<FixedType> Type() const override { return expr->Type(); }
};

struct Binary : public Expr {
  enum Op {
    EQEQ,
    NEQ,
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

  std::shared_ptr<FixedType> type;
  const Binary::Op op;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;

  Binary(std::shared_ptr<FixedType> type, Binary::Op op,
         std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs)
      : Expr(), type(type), op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::BINARY; }

  std::shared_ptr<FixedType> Type() const override { return type; }
};

struct Block : public Expr {
  std::shared_ptr<FixedType> type;
  unique_deque<Stmt> stmts;

  Block(std::shared_ptr<FixedType> type, unique_deque<Stmt> stmts)
      : Expr(), type(type), stmts(std::move(stmts)) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::BLOCK; }

  std::shared_ptr<FixedType> Type() const override { return type; }

  virtual bool IsTerminating() const override {
    for (auto &stmt : stmts) {
      if (stmt->IsTerminating()) return true;
    }
    return false;
  }
};

struct RetStmt : public Stmt {
  std::unique_ptr<Expr> expr;

  RetStmt(std::unique_ptr<Expr> expr = nullptr)
      : Stmt(), expr(std::move(expr)) {}

  // override Stmt
  Kind StmtKind() const override { return Stmt::Kind::RET; }

  virtual bool IsTerminating() const override { return true; }
};

struct VarDeclStmt : public Stmt {
  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  VarDeclStmt(std::shared_ptr<Decl> decl, std::unique_ptr<Expr> expr)
      : Stmt(), decl(decl), expr(std::move(expr)) {}

  // override Stmt
  Kind StmtKind() const override { return Stmt::Kind::VAR_DECL; }
};

struct AssignStmt : public Stmt {
  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  AssignStmt(std::shared_ptr<Decl> decl, std::unique_ptr<Expr> expr)
      : Stmt(), decl(decl), expr(std::move(expr)) {}

  // override Stmt
  Kind StmtKind() const override { return Stmt::Kind::ASSIGN; }
};

struct If : public Expr {
  std::shared_ptr<FixedType> type;
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Expr> els;
  /* bool as_stmt; */

  If(std::shared_ptr<FixedType> type, std::unique_ptr<Expr> cond,
     std::unique_ptr<Block> block, std::unique_ptr<Expr> els)
      : Expr(),
        type(type),
        cond(std::move(cond)),
        block(std::move(block)),
        els(std::move(els)) {}

  std::shared_ptr<FixedType> Type() const override { return type; }

  inline bool HasElse() const { return els != nullptr; }

  inline bool IfElseIf() const {
    assert(els);
    return els->ExprKind() == Expr::Kind::IF;
  }

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::IF; }

  virtual bool IsTerminating() const override {
    return block->IsTerminating() && HasElse() && els->IsTerminating();
  }
};

struct FnDecl : public HirNode {
  std::shared_ptr<Decl> decl;
  std::deque<std::shared_ptr<Decl>> args;
  std::unique_ptr<Block> block;

  FnDecl(std::shared_ptr<Decl> decl, std::deque<std::shared_ptr<Decl>> args,
         std::unique_ptr<Block> block)
      : decl(decl), args(args), block(std::move(block)) {}
};

struct Extern : public HirNode {
  std::shared_ptr<Decl> decl;

  Extern(std::shared_ptr<Decl> decl) : decl(decl) {}
};

struct File {
  unique_deque<Extern> externs;
  unique_deque<FnDecl> fn_decls;
};

}  // namespace hir

}  // namespace felis

#endif  // FELIS_NODE_HIR_H_
