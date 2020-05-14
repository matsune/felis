#ifndef FELIS_NODE_AST_H_
#define FELIS_NODE_AST_H_

#include <deque>
#include <memory>
#include <string>
#include <vector>

#include "loc.h"
#include "node/node.h"

namespace felis {

namespace ast {

struct Operator : public Node {
  enum Kind { UNARY, BINARY };
  virtual Operator::Kind OpKind() = 0;
};

struct UnaryOp : public Operator {
  enum Op { NEG, NOT };

  Loc begin;
  UnaryOp::Op op;

  UnaryOp(Loc begin, UnaryOp::Op op) : begin(begin), op(op) {}

  Operator::Kind OpKind() override { return Operator::Kind::UNARY; }

  Loc Begin() override { return begin; }

  Loc End() override { return begin; }
};

struct BinaryOp : public Operator {
  enum Op {
    LT = 1,
    LE = 2,
    GT = 3,
    GE = 4,

    ADD = 11,
    SUB = 12,

    MUL = 21,
    DIV = 22,
    MOD = 23
  };

  BinaryOp::Op op;
  Loc begin;

  BinaryOp(Loc begin, BinaryOp::Op op) : begin(begin), op(op) {}

  Operator::Kind OpKind() override { return Operator::Kind::BINARY; }

  Loc Begin() override { return begin; }

  Loc End() override {
    switch (op) {
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

struct Stmt : public Node {
  enum Kind { EXPR, RET, VAR_DECL, ASSIGN, IF, BLOCK };
  virtual Stmt::Kind StmtKind() = 0;
};

struct Block : public Stmt {
  Loc begin;
  Loc end;
  std::deque<std::unique_ptr<Stmt>> stmts;

  Block(Loc begin, Loc end, std::deque<std::unique_ptr<Stmt>> stmts)
      : begin(begin), end(end), stmts(std::move(stmts)) {}

  Stmt::Kind StmtKind() override { return Stmt::Kind::BLOCK; }

  Loc Begin() override { return begin; }

  Loc End() override { return end; }
};

struct Expr : public Stmt {
  enum Kind { IDENT, BINARY, LIT, CALL, UNARY };
  virtual Expr::Kind ExprKind() = 0;

  Stmt::Kind StmtKind() override { return Stmt::Kind::EXPR; }
};

struct Ident : public Expr {
  Loc begin;
  std::string val;

  Ident(Loc begin, std::string val) : begin(begin), val(val) {}

  Loc Begin() override { return begin; }

  Loc End() override { return begin + val.size(); }

  Expr::Kind ExprKind() override { return Expr::Kind::IDENT; }
};

struct Lit : public Expr {
  enum Kind { INT, FLOAT, CHAR, BOOL, STRING };
  Loc begin;
  Lit::Kind kind;
  std::string val;

  Lit(Loc begin, Lit::Kind kind, std::string val = "")
      : begin(begin), kind(kind), val(val) {}

  Lit::Kind LitKind() { return kind; }

  Loc Begin() override { return begin; }

  Loc End() override { return begin + val.size() + 2; }

  Expr::Kind ExprKind() override { return Expr::Kind::LIT; }
};

struct BinaryExpr : public Expr {
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  std::unique_ptr<BinaryOp> op;

  BinaryExpr(std::unique_ptr<Expr> lhs, std::unique_ptr<BinaryOp> op,
             std::unique_ptr<Expr> rhs)
      : lhs(std::move(lhs)), rhs(std::move(rhs)), op(std::move(op)) {}

  Loc Begin() override { return lhs->Begin(); }

  Loc End() override { return rhs->End(); }

  Expr::Kind ExprKind() override { return Expr::Kind::BINARY; }
};

struct CallExpr : public Expr {
  Loc end;
  std::unique_ptr<Ident> ident;
  std::deque<std::unique_ptr<Expr>> args;

  CallExpr(Loc end, std::unique_ptr<Ident> ident,
           std::deque<std::unique_ptr<Expr>> args)
      : end(end), ident(std::move(ident)), args(std::move(args)) {}

  Loc Begin() override { return ident->Begin(); }

  Loc End() override { return end; }

  Expr::Kind ExprKind() override { return Expr::Kind::CALL; }
};

struct UnaryExpr : public Expr {
  std::unique_ptr<UnaryOp> unOp;
  std::unique_ptr<Expr> expr;

  UnaryExpr(std::unique_ptr<UnaryOp> unOp, std::unique_ptr<Expr> expr)
      : unOp(std::move(unOp)), expr(std::move(expr)) {}

  Loc Begin() override { return unOp->Begin(); }

  Loc End() override { return expr->End(); }

  Expr::Kind ExprKind() override { return Expr::Kind::UNARY; }
};

struct RetStmt : public Stmt {
  Loc begin;
  std::unique_ptr<Expr> expr;

  RetStmt(Loc begin, std::unique_ptr<Expr> expr = nullptr)
      : begin(begin), expr(std::move(expr)) {}

  Loc Begin() override { return begin; }

  Loc End() override {
    if (expr)
      return expr->End();
    else
      return begin + 2;
  }

  Stmt::Kind StmtKind() override { return Stmt::Kind::RET; }
};

struct VarDeclStmt : public Stmt {
  Loc begin;
  bool isLet;
  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  VarDeclStmt(Loc begin, bool isLet, std::unique_ptr<Ident> name,
              std::unique_ptr<Expr> expr)
      : begin(begin),
        isLet(isLet),
        name(std::move(name)),
        expr(std::move(expr)) {}

  Loc Begin() override { return begin; }

  Loc End() override { return expr->End(); }

  Stmt::Kind StmtKind() override { return Stmt::Kind::VAR_DECL; }
};

struct AssignStmt : public Stmt {
  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  AssignStmt(std::unique_ptr<Ident> name, std::unique_ptr<Expr> expr)
      : name(std::move(name)), expr(std::move(expr)) {}

  Loc Begin() override { return name->Begin(); }

  Loc End() override { return expr->End(); }

  Stmt::Kind StmtKind() override { return Stmt::Kind::ASSIGN; }
};

struct IfStmt : public Stmt {
  Loc begin;
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Stmt> els;  // block or if

  IfStmt(Loc begin, std::unique_ptr<Expr> cond, std::unique_ptr<Block> block,
         std::unique_ptr<Stmt> els = nullptr)
      : begin(begin),
        cond(std::move(cond)),
        block(std::move(block)),
        els(std::move(els)) {}

  Loc Begin() override { return begin; }

  Loc End() override {
    if (els) {
      return els->End();
    } else {
      return block->End();
    }
  }

  Stmt::Kind StmtKind() override { return Stmt::Kind::IF; }
};

struct FnArg : public Node {
  std::unique_ptr<Ident> ty;
  std::unique_ptr<Ident> name;

  FnArg(std::unique_ptr<Ident> ty, std::unique_ptr<Ident> name = nullptr)
      : ty(std::move(ty)), name(std::move(name)) {}

  bool WithName() { return name != nullptr; }

  Loc Begin() override { return WithName() ? name->Begin() : ty->Begin(); }

  Loc End() override { return ty->End(); }
};

struct FnArgs : public Node {
  Loc begin;
  Loc end;
  std::vector<std::unique_ptr<FnArg>> list;

  FnArgs(Loc begin, Loc end, std::vector<std::unique_ptr<FnArg>> list)
      : begin(begin), end(end), list(std::move(list)) {}

  Loc Begin() override { return begin; }

  Loc End() override { return end; }
};

struct FnProto : public Node {
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

  Loc Begin() override { return begin; }

  Loc End() override { return args->End(); }
};

struct FnDecl : public Node {
  std::unique_ptr<FnProto> proto;
  std::unique_ptr<Block> block;

  FnDecl(std::unique_ptr<FnProto> proto, std::unique_ptr<Block> block)
      : proto(std::move(proto)), block(std::move(block)) {}

  Loc Begin() override { return proto->Begin(); }

  Loc End() override { return block->End(); }
};

struct Extern : public Node {
  Loc begin;
  std::unique_ptr<FnProto> proto;

  Extern(Loc begin, std::unique_ptr<FnProto> proto)
      : begin(begin), proto(std::move(proto)) {}

  Loc Begin() override { return begin; }

  Loc End() override { return proto->End(); }
};

struct File {
  std::vector<std::unique_ptr<Extern>> externs;
  std::deque<std::unique_ptr<FnDecl>> fnDecls;
};

}  // namespace ast

}  // namespace felis

#endif  // FELIS_NODE_AST_H_
