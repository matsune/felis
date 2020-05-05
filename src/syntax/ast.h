#ifndef FELIS_SYNTAX_AST_H_
#define FELIS_SYNTAX_AST_H_

#include <deque>
#include <memory>
#include <string>
#include <vector>

#include "syntax/pos.h"

namespace felis {

namespace ast {

enum BinOp {
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

struct Node {
  enum Kind { EXTERN, FN_DECL, FN_PROTO, FN_ARG, STMT };
  virtual Kind NodeKind() = 0;

  virtual Pos GetPos() = 0;
};

struct Stmt : public Node {
  Node::Kind NodeKind() override { return Node::Kind::STMT; }

  enum Kind { EXPR, RET, VAR_DECL, ASSIGN, IF, BLOCK };
  virtual Stmt::Kind StmtKind() = 0;
};

struct Block : public Stmt {
  Block(Pos pos) : pos(pos){};

  Pos pos;
  std::deque<std::unique_ptr<Stmt>> stmts;

  Pos GetPos() override { return pos; }

  Stmt::Kind StmtKind() override { return Stmt::Kind::BLOCK; }
};

struct Expr : public Stmt {
  enum Kind { IDENT, BINARY, LIT, CALL, UNARY };
  virtual Expr::Kind ExprKind() = 0;

  Stmt::Kind StmtKind() override { return Stmt::Kind::EXPR; }
};

struct Ident : public Expr {
  Ident(Pos pos, std::string val) : pos(pos), val(val) {}

  Pos pos;
  std::string val;

  Pos GetPos() override { return pos; }

  Expr::Kind ExprKind() override { return Expr::Kind::IDENT; }
};

struct Lit : public Expr {
  enum Kind { INT, FLOAT, CHAR, BOOL, STRING };
  Lit::Kind LitKind() { return kind; }

  Lit(Pos pos, Lit::Kind kind, std::string val = "")
      : pos(pos), kind(kind), val(val) {}

  Pos pos;
  Lit::Kind kind;
  std::string val;

  Pos GetPos() override { return pos; }

  Expr::Kind ExprKind() override { return Expr::Kind::LIT; }
};

enum UnOp { NEG, NOT };

struct BinaryExpr : public Expr {
  BinaryExpr(std::unique_ptr<Expr> lhs, BinOp op, std::unique_ptr<Expr> rhs)
      : lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  BinOp op;

  Pos GetPos() override {
    if (!lhs) {
      std::cerr << "lhs is already moved" << std::endl;
      std::exit(1);
    }
    return lhs->GetPos();
  }

  Expr::Kind ExprKind() override { return Expr::Kind::BINARY; }
};

struct CallExpr : public Expr {
  CallExpr(std::unique_ptr<Ident> ident, std::deque<std::unique_ptr<Expr>> args)
      : ident(std::move(ident)), args(std::move(args)) {}

  std::unique_ptr<Ident> ident;
  std::deque<std::unique_ptr<Expr>> args;

  Pos GetPos() override {
    if (!ident) {
      std::cerr << "ident is already moved" << std::endl;
      std::exit(1);
    }
    return ident->pos;
  }

  Expr::Kind ExprKind() override { return Expr::Kind::CALL; }
};

struct UnaryExpr : public Expr {
  UnaryExpr(Pos pos, UnOp unOp, std::unique_ptr<Expr> expr)
      : pos(pos), unOp(unOp), expr(std::move(expr)) {}

  Pos pos;
  UnOp unOp;
  std::unique_ptr<Expr> expr;

  Pos GetPos() override { return pos; }

  Expr::Kind ExprKind() override { return Expr::Kind::UNARY; }
};

struct RetStmt : public Stmt {
  RetStmt(Pos pos, std::unique_ptr<Expr> expr = nullptr)
      : pos(pos), expr(std::move(expr)) {}

  Pos pos;
  std::unique_ptr<Expr> expr;

  Pos GetPos() override { return pos; }

  Stmt::Kind StmtKind() override { return Stmt::Kind::RET; }
};

struct VarDeclStmt : public Stmt {
  VarDeclStmt(Pos pos, bool isLet, std::unique_ptr<Ident> name,
              std::unique_ptr<Expr> expr)
      : pos(pos), isLet(isLet), name(std::move(name)), expr(std::move(expr)) {}

  Pos pos;
  bool isLet;
  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  Pos GetPos() override { return pos; }

  Stmt::Kind StmtKind() override { return Stmt::Kind::VAR_DECL; }
};

struct AssignStmt : public Stmt {
  AssignStmt(std::unique_ptr<Ident> name, std::unique_ptr<Expr> expr)
      : name(std::move(name)), expr(std::move(expr)) {}

  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  Pos GetPos() override {
    if (!name) {
      std::cerr << "name is already moved" << std::endl;
      std::exit(1);
    }
    return name->pos;
  }

  Stmt::Kind StmtKind() override { return Stmt::Kind::ASSIGN; }
};

struct IfStmt : public Stmt {
  IfStmt(Pos pos, std::unique_ptr<Expr> cond, std::unique_ptr<Block> block,
         std::unique_ptr<Stmt> els = nullptr)
      : pos(pos),
        cond(std::move(cond)),
        block(std::move(block)),
        els(std::move(els)) {}

  Pos pos;
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Stmt> els;  // block or if

  Pos GetPos() override { return pos; }

  Stmt::Kind StmtKind() override { return Stmt::Kind::IF; }
};

struct FnArg : public Node {
  FnArg(std::unique_ptr<Ident> ty, std::unique_ptr<Ident> name = nullptr)
      : ty(std::move(ty)), name(std::move(name)) {}

  std::unique_ptr<Ident> ty;
  std::unique_ptr<Ident> name;

  bool WithName() { return name != nullptr; }

  Pos GetPos() override {
    if (WithName()) {
      if (!name) {
        std::cerr << "name is already moved" << std::endl;
        std::exit(1);
      }
      return name->pos;
    } else {
      if (!ty) {
        std::cerr << "ty is already moved" << std::endl;
        std::exit(1);
      }
      return ty->pos;
    }
  }

  Node::Kind NodeKind() override { return Kind::FN_ARG; }
};

struct FnProto : public Node {
  FnProto(Pos pos, std::unique_ptr<Ident> name,
          std::vector<std::unique_ptr<FnArg>> args,
          std::unique_ptr<Ident> ret = nullptr)
      : pos(pos),
        name(std::move(name)),
        args(std::move(args)),
        ret(std::move(ret)) {}

  Pos pos;
  std::unique_ptr<Ident> name;
  std::vector<std::unique_ptr<FnArg>> args;
  std::unique_ptr<Ident> ret;

  Pos GetPos() override { return pos; }

  Node::Kind NodeKind() override { return Kind::FN_PROTO; }
};

struct FnDecl : public Node {
  FnDecl(std::unique_ptr<FnProto> proto, std::unique_ptr<Block> block)
      : proto(std::move(proto)), block(std::move(block)) {}

  std::unique_ptr<FnProto> proto;
  std::unique_ptr<Block> block;

  Pos GetPos() override {
    if (!proto) {
      std::cerr << "proto is already moved" << std::endl;
      std::exit(1);
    }
    return proto->pos;
  }

  Node::Kind NodeKind() override { return Kind::FN_DECL; }
};

struct Extern : public Node {
  Extern(Pos pos, std::unique_ptr<FnProto> proto)
      : pos(pos), proto(std::move(proto)) {}

  Pos pos;
  std::unique_ptr<FnProto> proto;

  Pos GetPos() override { return pos; }

  Node::Kind NodeKind() override { return Kind::EXTERN; }
};

struct File {
  std::vector<std::unique_ptr<Extern>> externs;
  std::deque<std::unique_ptr<FnDecl>> fnDecls;
};

}  // namespace ast

}  // namespace felis

#endif  // FELIS_SYNTAX_AST_H_
