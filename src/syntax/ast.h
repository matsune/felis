#ifndef FELIS_SYNTAX_AST_H_
#define FELIS_SYNTAX_AST_H_

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
  virtual ~Node() = default;

  virtual Kind NodeKind() = 0;
  virtual Pos GetPos() = 0;
};

struct Stmt : public Node {
  Node::Kind NodeKind() override { return Node::Kind::STMT; }

  enum Kind { EXPR, RET, VAR_DECL, ASSIGN, IF, BLOCK };

  virtual Kind StmtKind() = 0;
  virtual Pos GetPos() override = 0;
};

struct Block : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::BLOCK; }

  Pos pos;
  std::vector<std::unique_ptr<Stmt>> stmts;

  explicit Block(Pos pos) : pos(pos){};

  Pos GetPos() override { return pos; }
};

struct Expr : public Stmt {
  Stmt::Kind StmtKind() override { return Stmt::Kind::EXPR; }

  enum Kind { IDENT, BINARY, LIT, CALL, UNARY };

  virtual Kind ExprKind() = 0;
  virtual Pos GetPos() override = 0;
};

struct Ident : public Expr {
  Expr::Kind ExprKind() override { return Expr::Kind::IDENT; }

  std::string val;
  Pos pos;

  explicit Ident(Pos pos, std::string val) : pos(pos), val(val) {}

  Pos GetPos() override { return pos; }
};

struct Lit : public Expr {
  Expr::Kind ExprKind() override { return Expr::Kind::LIT; }

  enum Kind { INT, FLOAT, CHAR, BOOL, STRING };

  Pos pos;
  Kind kind;

  std::string val;

  explicit Lit(Pos pos, Kind kind, std::string val = "")
      : pos(pos), kind(kind), val(val) {}

  Pos GetPos() override { return pos; }

  Lit::Kind LitKind() { return kind; }
};

enum UnOp { NEG, NOT };

struct BinaryExpr : public Expr {
  Expr::Kind ExprKind() override { return Expr::Kind::BINARY; }

  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  BinOp op;

  BinaryExpr(Expr* lhs, BinOp op, Expr* rhs)
      : lhs(std::unique_ptr<Expr>(lhs)),
        rhs(std::unique_ptr<Expr>(rhs)),
        op(op) {}

  Pos GetPos() override { return lhs->GetPos(); }
};

struct CallExpr : public Expr {
  Expr::Kind ExprKind() override { return Expr::Kind::CALL; }

  std::unique_ptr<Ident> ident;
  std::vector<std::unique_ptr<Expr>> args;

  CallExpr(std::unique_ptr<Ident> ident,
           std::vector<std::unique_ptr<Expr>> args)
      : ident(std::move(ident)), args(std::move(args)) {}

  Pos GetPos() override { return ident->pos; }
};

struct UnaryExpr : public Expr {
  Expr::Kind ExprKind() override { return Expr::Kind::UNARY; }

  Pos pos;
  UnOp unOp;
  std::unique_ptr<Expr> expr;

  UnaryExpr(Pos pos, UnOp unOp, Expr* expr)
      : pos(pos), unOp(unOp), expr(std::unique_ptr<Expr>(expr)) {}

  Pos GetPos() override { return pos; }
};

struct RetStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::RET; }

  Pos pos;
  std::unique_ptr<Expr> expr;

  explicit RetStmt(Pos pos, Expr* expr = nullptr)
      : pos(pos), expr(std::unique_ptr<Expr>(expr)) {}

  Pos GetPos() override { return pos; }
};

struct VarDeclStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::VAR_DECL; }

  bool isLet;
  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;
  Pos pos;

  VarDeclStmt(Pos pos, bool isLet, std::unique_ptr<Ident> name, Expr* expr)
      : pos(pos),
        isLet(isLet),
        name(std::move(name)),
        expr(std::unique_ptr<Expr>(expr)) {}

  Pos GetPos() override { return pos; }
};

struct AssignStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::ASSIGN; }

  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  AssignStmt(std::unique_ptr<Ident> name, Expr* expr)
      : name(std::move(name)), expr(std::unique_ptr<Expr>(expr)) {}

  Pos GetPos() override { return name->pos; }
};

struct IfStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::IF; }

  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Stmt> els;  // block or if
  Pos pos;

  IfStmt(Pos pos, Expr* cond, std::unique_ptr<Block> block,
         std::unique_ptr<Stmt> els = nullptr)
      : pos(pos),
        cond(std::unique_ptr<Expr>(cond)),
        block(std::move(block)),
        els(std::move(els)) {}

  Pos GetPos() override { return pos; }
};

struct FnArg : public Node {
  Kind NodeKind() override { return Kind::FN_ARG; }

  std::unique_ptr<Ident> ty;
  std::unique_ptr<Ident> name;

  bool withName() { return name != nullptr; }

  FnArg(std::unique_ptr<Ident> ty, std::unique_ptr<Ident> name = nullptr)
      : ty(std::move(ty)), name(std::move(name)) {}

  Pos GetPos() override { return withName() ? name->pos : ty->pos; }
};

struct FnProto : public Node {
  Kind NodeKind() override { return Kind::FN_PROTO; }

  std::unique_ptr<Ident> name;
  std::vector<std::unique_ptr<FnArg>> args;
  std::unique_ptr<Ident> ret;
  Pos pos;

  FnProto(Pos pos, std::unique_ptr<Ident> name,
          std::vector<std::unique_ptr<FnArg>> args,
          std::unique_ptr<Ident> ret = nullptr)
      : pos(pos),
        name(std::move(name)),
        args(std::move(args)),
        ret(std::move(ret)) {}

  Pos GetPos() override { return pos; }
};

struct FnDecl : public Node {
  Kind NodeKind() override { return Kind::FN_DECL; }

  std::unique_ptr<FnProto> proto;
  std::unique_ptr<Block> block;

  explicit FnDecl(std::unique_ptr<FnProto> proto, std::unique_ptr<Block> block)
      : proto(std::move(proto)), block(std::move(block)) {}

  Pos GetPos() override { return proto->pos; }
};

struct Extern : public Node {
  Kind NodeKind() override { return Kind::EXTERN; }
  std::unique_ptr<FnProto> proto;
  Pos pos;

  explicit Extern(Pos pos, std::unique_ptr<FnProto> proto)
      : pos(pos), proto(std::move(proto)) {}

  Pos GetPos() override { return pos; }
};

struct File {
  std::vector<std::unique_ptr<Extern>> externs;
  std::vector<std::unique_ptr<FnDecl>> fnDecls;
};

}  // namespace ast

}  // namespace felis

#endif  // FELIS_SYNTAX_AST_H_
