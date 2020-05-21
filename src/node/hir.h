#ifndef FELIS_NODE_HIR_H_
#define FELIS_NODE_HIR_H_

#include "check/decl.h"
#include "check/type.h"
#include "macro.h"
#include "node/ast.h"
#include "node/node.h"
#include "syntax/rune.h"

namespace felis {

namespace hir {

struct HirNode : public Node {
  Loc begin;
  Loc end;
  HirNode(Loc begin, Loc end) : begin(begin), end(end) {}

  Loc Begin() const override { return begin; }
  Loc End() const override { return end; }
};

struct Stmt : HirNode {
  enum Kind { EXPR, RET, VAR_DECL, ASSIGN };
  virtual Kind StmtKind() const = 0;

  Stmt(Loc begin, Loc end) : HirNode(begin, end) {}
};

struct Expr : public Stmt {
  Expr(Loc begin, Loc end) : Stmt(begin, end) {}

  enum Kind { BINARY, VALUE, CALL, UNARY, IF, BLOCK };
  virtual Expr::Kind ExprKind() const = 0;
  virtual std::shared_ptr<Typed> Type() const = 0;

  // override Stmt
  Stmt::Kind StmtKind() const override { return Stmt::Kind::EXPR; }
};

struct Value : public Expr {
  Value(Loc begin, Loc end) : Expr(begin, end) {}

  enum Kind { CONSTANT, VARIABLE };
  virtual Value::Kind ValueKind() const = 0;

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::VALUE; }
};

struct Variable : public Value {
  std::shared_ptr<Decl> decl;

  Variable(Loc begin, Loc end, std::shared_ptr<Decl> decl)
      : Value(begin, end), decl(decl){};

  // override Value
  Value::Kind ValueKind() const override { return Value::Kind::VARIABLE; }

  // override Expr
  std::shared_ptr<Typed> Type() const override {
    return std::dynamic_pointer_cast<Typed>(decl->type);
  }
};

template <typename T>
struct Constant : public Value {
  std::shared_ptr<Typed> type;
  T val;

  Constant(Loc begin, Loc end, std::shared_ptr<Typed> type, T val)
      : Value(begin, end), type(type), val(val) {}

  // override Value
  Value::Kind ValueKind() const override { return Value::Kind::CONSTANT; }

  // override Expr
  std::shared_ptr<Typed> Type() const override { return type; }
};

using IntConstant = Constant<int64_t>;
using FloatConstant = Constant<double>;
using BoolConstant = Constant<bool>;
using StringConstant = Constant<std::string>;

struct Call : public Expr {
  std::shared_ptr<Decl> decl;
  unique_deque<Expr> args;

  Call(Loc begin, Loc end, std::shared_ptr<Decl> decl, unique_deque<Expr> args)
      : Expr(begin, end), decl(decl), args(std::move(args)) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::CALL; }

  // override Expr
  std::shared_ptr<Typed> Type() const override {
    return std::dynamic_pointer_cast<Typed>(decl->AsFuncType()->ret);
  }
};

struct Unary : public Expr {
  enum Op { NEG, NOT };

  Unary::Op op;
  std::unique_ptr<Expr> expr;

  Unary(Loc begin, Loc end, Unary::Op op, std::unique_ptr<Expr> expr)
      : Expr(begin, end), op(op), expr(std::move(expr)) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::UNARY; }

  std::shared_ptr<Typed> Type() const override { return expr->Type(); }
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

  std::shared_ptr<Typed> type;
  Binary::Op op;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;

  Binary(Loc begin, Loc end, std::shared_ptr<Typed> type, Binary::Op op,
         std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs)
      : Expr(begin, end),
        type(type),
        op(op),
        lhs(std::move(lhs)),
        rhs(std::move(rhs)) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::BINARY; }

  std::shared_ptr<Typed> Type() const override { return type; }
};

struct Block : public Expr {
  std::shared_ptr<Typed> type;
  unique_deque<Stmt> stmts;

  Block(Loc begin, Loc end, std::shared_ptr<Typed> type,
        unique_deque<Stmt> stmts)
      : Expr(begin, end), type(type), stmts(std::move(stmts)) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::BLOCK; }

  std::shared_ptr<Typed> Type() const override { return type; }
};

struct RetStmt : public Stmt {
  std::unique_ptr<Expr> expr;

  RetStmt(Loc begin, Loc end, std::unique_ptr<Expr> expr = nullptr)
      : Stmt(begin, end), expr(std::move(expr)) {}

  // override Stmt
  Kind StmtKind() const override { return Stmt::Kind::RET; }
};

struct VarDeclStmt : public Stmt {
  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  VarDeclStmt(Loc begin, Loc end, std::shared_ptr<Decl> decl,
              std::unique_ptr<Expr> expr)
      : Stmt(begin, end), decl(decl), expr(std::move(expr)) {}

  // override Stmt
  Kind StmtKind() const override { return Stmt::Kind::VAR_DECL; }
};

struct AssignStmt : public Stmt {
  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  AssignStmt(Loc begin, Loc end, std::shared_ptr<Decl> decl,
             std::unique_ptr<Expr> expr)
      : Stmt(begin, end), decl(decl), expr(std::move(expr)) {}

  // override Stmt
  Kind StmtKind() const override { return Stmt::Kind::ASSIGN; }
};

struct If : public Expr {
  std::shared_ptr<Typed> type;
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Expr> els;  // null or Block or If

  If(Loc begin, Loc end, std::shared_ptr<Typed> type,
     std::unique_ptr<Expr> cond, std::unique_ptr<Block> block,
     std::unique_ptr<Expr> els)
      : Expr(begin, end),
        type(type),
        cond(std::move(cond)),
        block(std::move(block)),
        els(std::move(els)) {}

  std::shared_ptr<Typed> Type() const override { return type; }

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::IF; }
};

struct FnDecl : public HirNode {
  std::shared_ptr<Decl> decl;
  std::deque<std::shared_ptr<Decl>> args;
  std::unique_ptr<Block> block;

  FnDecl(Loc begin, Loc end, std::shared_ptr<Decl> decl,
         std::deque<std::shared_ptr<Decl>> args, std::unique_ptr<Block> block)
      : HirNode(begin, end), decl(decl), args(args), block(std::move(block)) {}
};

struct Extern : public HirNode {
  std::shared_ptr<Decl> decl;

  Extern(Loc begin, Loc end, std::shared_ptr<Decl> decl)
      : HirNode(begin, end), decl(decl) {}
};

struct File {
  std::vector<std::unique_ptr<Extern>> externs;
  unique_deque<FnDecl> fn_decls;
};

}  // namespace hir

}  // namespace felis

#endif  // FELIS_NODE_HIR_H_
