#ifndef FELIS_NODE_AST_H_
#define FELIS_NODE_AST_H_

#include <memory>
#include <string>
#include <vector>

#include "loc.h"
#include "node/node.h"
#include "unique.h"

namespace felis {

namespace ast {

struct AstNode : public Node {};

struct Operator : public AstNode {
  enum Kind { UNARY, BINARY };
  virtual Operator::Kind OpKind() const = 0;
};

struct UnaryOp : public Operator {
  enum Op { NEG, NOT };

  Loc begin;
  UnaryOp::Op op;

  UnaryOp(Loc begin, UnaryOp::Op op) : begin(begin), op(op) {}

  Operator::Kind OpKind() const override { return Operator::Kind::UNARY; }

  Loc Begin() const override { return begin; }

  Loc End() const override { return begin; }
};

struct BinaryOp : public Operator {
  enum Op {
    EQEQ = 1,
    NEQ = 2,
    LT = 3,
    LE = 4,
    GT = 5,
    GE = 6,

    ADD = 11,
    SUB = 12,

    MUL = 21,
    DIV = 22,
    MOD = 23
  };

  BinaryOp::Op op;
  Loc begin;

  BinaryOp(Loc begin, BinaryOp::Op op) : begin(begin), op(op) {}

  Operator::Kind OpKind() const override { return Operator::Kind::BINARY; }

  Loc Begin() const override { return begin; }

  Loc End() const override {
    switch (op) {
      case BinaryOp::Op::EQEQ:
      case BinaryOp::Op::NEQ:
        return begin + 2;
      case BinaryOp::Op::LT:
      case BinaryOp::Op::LE:
      case BinaryOp::Op::GT:
      case BinaryOp::Op::GE:
        return begin + 1;
      case BinaryOp::Op::ADD:
      case BinaryOp::Op::SUB:
      case BinaryOp::Op::MUL:
      case BinaryOp::Op::DIV:
      case BinaryOp::Op::MOD:
        return begin;
    }
  }
};

struct Stmt : public AstNode {
  enum Kind { EXPR, RET, VAR_DECL, ASSIGN };
  virtual Stmt::Kind StmtKind() const = 0;

  bool IsRet() const { return StmtKind() == Kind::RET; }
};

struct Expr : public Stmt {
  enum Kind { IDENT, BINARY, LIT, CALL, UNARY, BLOCK, IF };
  virtual Expr::Kind ExprKind() const = 0;

  Stmt::Kind StmtKind() const override { return Stmt::Kind::EXPR; }
};

struct Block : public Expr {
  Loc begin;
  Loc end;
  unique_deque<Stmt> stmts;

  Block(Loc begin, Loc end, unique_deque<Stmt> stmts)
      : begin(begin), end(end), stmts(std::move(stmts)) {}

  Expr::Kind ExprKind() const override { return Expr::Kind::BLOCK; }

  Loc Begin() const override { return begin; }

  Loc End() const override { return end; }

  bool HasRet() const {
    for (auto &stmt : stmts) {
      if (stmt->IsRet()) return true;
    }
    return false;
  }
};

struct If : public Expr {
  Loc begin;
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Expr> els;  // block or if

  If(Loc begin, std::unique_ptr<Expr> cond, std::unique_ptr<Block> block,
     std::unique_ptr<Expr> els = nullptr)
      : begin(begin),
        cond(std::move(cond)),
        block(std::move(block)),
        els(std::move(els)) {}

  inline bool HasElse() const { return els != nullptr; }

  inline bool IsElseBlock() const {
    if (els)
      return els->ExprKind() == Expr::Kind::BLOCK;
    else
      return false;
  }

  inline bool IsElseIf() const {
    if (els)
      return els->ExprKind() == Expr::Kind::IF;
    else
      return false;
  }

  Expr::Kind ExprKind() const override { return Expr::Kind::IF; }

  Loc Begin() const override { return begin; }

  Loc End() const override {
    if (els) {
      return els->End();
    } else {
      return block->End();
    }
  }
};

struct Ident : public Expr {
  Loc begin;
  std::string val;

  Ident(Loc begin, std::string val) : begin(begin), val(val) {}

  Loc Begin() const override { return begin; }

  Loc End() const override { return begin + val.size(); }

  Expr::Kind ExprKind() const override { return Expr::Kind::IDENT; }
};

struct Lit : public Expr {
  enum Kind { INT, FLOAT, CHAR, BOOL, STRING };
  Loc begin;
  Lit::Kind kind;
  std::string val;

  Lit(Loc begin, Lit::Kind kind, std::string val = "")
      : begin(begin), kind(kind), val(val) {}

  Lit::Kind LitKind() { return kind; }

  Loc Begin() const override { return begin; }

  Loc End() const override { return begin + val.size() + 2; }

  Expr::Kind ExprKind() const override { return Expr::Kind::LIT; }
};

struct BinaryExpr : public Expr {
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  std::unique_ptr<BinaryOp> op;

  BinaryExpr(std::unique_ptr<Expr> lhs, std::unique_ptr<BinaryOp> op,
             std::unique_ptr<Expr> rhs)
      : lhs(std::move(lhs)), rhs(std::move(rhs)), op(std::move(op)) {}

  Loc Begin() const override { return lhs->Begin(); }

  Loc End() const override { return rhs->End(); }

  Expr::Kind ExprKind() const override { return Expr::Kind::BINARY; }
};

struct CallExpr : public Expr {
  Loc end;
  std::unique_ptr<Ident> ident;
  unique_deque<Expr> args;

  CallExpr(Loc end, std::unique_ptr<Ident> ident, unique_deque<Expr> args)
      : end(end), ident(std::move(ident)), args(std::move(args)) {}

  Loc Begin() const override { return ident->Begin(); }

  Loc End() const override { return end; }

  Expr::Kind ExprKind() const override { return Expr::Kind::CALL; }
};

struct UnaryExpr : public Expr {
  std::unique_ptr<UnaryOp> op;
  std::unique_ptr<Expr> expr;

  UnaryExpr(std::unique_ptr<UnaryOp> op, std::unique_ptr<Expr> expr)
      : op(std::move(op)), expr(std::move(expr)) {}

  Loc Begin() const override { return op->Begin(); }

  Loc End() const override { return expr->End(); }

  Expr::Kind ExprKind() const override { return Expr::Kind::UNARY; }
};

struct RetStmt : public Stmt {
  Loc begin;
  std::unique_ptr<Expr> expr;

  RetStmt(Loc begin, std::unique_ptr<Expr> expr = nullptr)
      : begin(begin), expr(std::move(expr)) {}

  Loc Begin() const override { return begin; }

  Loc End() const override {
    if (expr)
      return expr->End();
    else
      return begin + 2;
  }

  Stmt::Kind StmtKind() const override { return Stmt::Kind::RET; }
};

struct VarDeclStmt : public Stmt {
  Loc begin;
  bool is_let;
  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  VarDeclStmt(Loc begin, bool is_let, std::unique_ptr<Ident> name,
              std::unique_ptr<Expr> expr)
      : begin(begin),
        is_let(is_let),
        name(std::move(name)),
        expr(std::move(expr)) {}

  Loc Begin() const override { return begin; }

  Loc End() const override { return expr->End(); }

  Stmt::Kind StmtKind() const override { return Stmt::Kind::VAR_DECL; }
};

struct AssignStmt : public Stmt {
  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  AssignStmt(std::unique_ptr<Ident> name, std::unique_ptr<Expr> expr)
      : name(std::move(name)), expr(std::move(expr)) {}

  Loc Begin() const override { return name->Begin(); }

  Loc End() const override { return expr->End(); }

  Stmt::Kind StmtKind() const override { return Stmt::Kind::ASSIGN; }
};

struct FnArg : public AstNode {
  std::unique_ptr<Ident> ty;
  std::unique_ptr<Ident> name;

  FnArg(std::unique_ptr<Ident> ty, std::unique_ptr<Ident> name = nullptr)
      : ty(std::move(ty)), name(std::move(name)) {}

  bool WithName() const { return name != nullptr; }

  Loc Begin() const override {
    return WithName() ? name->Begin() : ty->Begin();
  }

  Loc End() const override { return ty->End(); }
};

struct FnArgs : public AstNode {
  Loc begin;
  Loc end;
  std::vector<std::unique_ptr<FnArg>> list;

  FnArgs(Loc begin, Loc end, std::vector<std::unique_ptr<FnArg>> list)
      : begin(begin), end(end), list(std::move(list)) {}

  Loc Begin() const override { return begin; }

  Loc End() const override { return end; }
};

struct FnProto : public AstNode {
  Loc begin;
  std::unique_ptr<Ident> name;
  std::unique_ptr<FnArgs> args;
  std::unique_ptr<Ident> ret;

  FnProto(Loc begin, std::unique_ptr<Ident> name, std::unique_ptr<FnArgs> args,
          std::unique_ptr<Ident> ret = nullptr)
      : begin(begin),
        name(std::move(name)),
        args(std::move(args)),
        ret(std::move(ret)) {}

  Loc Begin() const override { return begin; }

  Loc End() const override {
    if (ret)
      return ret->End();
    else
      return args->End();
  }
};

struct FnDecl : public AstNode {
  std::unique_ptr<FnProto> proto;
  std::unique_ptr<Block> block;

  FnDecl(std::unique_ptr<FnProto> proto, std::unique_ptr<Block> block)
      : proto(std::move(proto)), block(std::move(block)) {}

  Loc Begin() const override { return proto->Begin(); }

  Loc End() const override { return block->End(); }
};

struct Extern : public AstNode {
  Loc begin;
  std::unique_ptr<FnProto> proto;

  Extern(Loc begin, std::unique_ptr<FnProto> proto)
      : begin(begin), proto(std::move(proto)) {}

  Loc Begin() const override { return begin; }

  Loc End() const override { return proto->End(); }
};

struct File {
  unique_deque<Extern> externs;
  unique_deque<FnDecl> fn_decls;
};

}  // namespace ast

}  // namespace felis

#endif  // FELIS_NODE_AST_H_
