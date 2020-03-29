#ifndef FELIS_SYNTAX_AST_H_
#define FELIS_SYNTAX_AST_H_

#include <memory>
#include <string>
#include <vector>

#include "syntax/pos.h"

namespace felis {

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

using NodeId = uint32_t;

struct Node {
  enum Kind { EXTERN, FN_DECL, FN_PROTO, FN_ARG, STMT };
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

  std::string sval;
  Pos pos;

  explicit Ident(Pos pos, std::string sval = "") : pos(pos), sval(sval) {}

  Pos GetPos() override { return pos; }
};

struct Lit : public Expr {
  Expr::Kind ExprKind() override { return Expr::Kind::LIT; }

  enum Kind { INT, FLOAT, BOOL, CHAR, STR };

  virtual Kind LitKind() = 0;
  virtual Pos GetPos() override = 0;
};

struct LitInt : public Lit {
  Kind LitKind() override { return Kind::INT; }

  Pos pos;
  uint64_t ival;

  explicit LitInt(Pos pos, uint64_t ival = 0) : pos(pos), ival(ival) {}

  Pos GetPos() override { return pos; }
};

struct LitBool : public Lit {
  Kind LitKind() override { return Kind::BOOL; }

  Pos pos;
  bool bval;

  explicit LitBool(Pos pos, bool bval = false) : pos(pos), bval(bval) {}

  Pos GetPos() override { return pos; }
};

struct LitFloat : public Lit {
  Kind LitKind() override { return Kind::FLOAT; }

  Pos pos;
  double fval;

  explicit LitFloat(Pos pos, double fval) : pos(pos), fval(fval) {}

  Pos GetPos() override { return pos; }
};

struct LitStr : public Lit {
  Kind LitKind() override { return Kind::STR; }

  Pos pos;
  std::string sval;

  explicit LitStr(Pos pos, std::string sval) : pos(pos), sval(sval) {}

  Pos GetPos() override { return pos; }
};

struct LitChar : public Lit {
  Kind LitKind() override { return Kind::CHAR; }

  Pos pos;
  char cval;

  explicit LitChar(Pos pos, char cval = 0) : pos(pos), cval(cval) {}

  Pos GetPos() override { return pos; }
};

enum UnOp { NEG, NOT };

struct BinaryExpr : public Expr {
  Expr::Kind ExprKind() override { return Expr::Kind::BINARY; }

  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  BinOp op;

  BinaryExpr(std::unique_ptr<Expr> lhs, BinOp op, std::unique_ptr<Expr> rhs)
      : lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

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
  std::unique_ptr<UnOp> unOp;
  std::unique_ptr<Expr> expr;

  UnaryExpr(Pos pos, std::unique_ptr<UnOp> unOp, std::unique_ptr<Expr> expr)
      : pos(pos), unOp(std::move(unOp)), expr(std::move(expr)) {}

  Pos GetPos() override { return pos; }
};

struct RetStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::RET; }

  Pos pos;
  std::unique_ptr<Expr> expr;

  explicit RetStmt(Pos pos,
                   std::unique_ptr<Expr> expr = std::unique_ptr<Expr>())
      : pos(pos), expr(std::move(expr)) {}

  Pos GetPos() override { return pos; }
};

struct VarDeclStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::VAR_DECL; }

  bool isLet;
  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;
  Pos pos;

  VarDeclStmt(Pos pos, bool isLet, std::unique_ptr<Ident> name,
              std::unique_ptr<Expr> expr)
      : pos(pos), isLet(isLet), name(std::move(name)), expr(std::move(expr)) {}

  Pos GetPos() override { return pos; }
};

struct AssignStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::ASSIGN; }

  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  AssignStmt(std::unique_ptr<Ident> name, std::unique_ptr<Expr> expr)
      : name(std::move(name)), expr(std::move(expr)) {}

  Pos GetPos() override { return name->pos; }
};

struct IfStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::IF; }

  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Stmt> els;  // block or if
  Pos pos;

  IfStmt(Pos pos, std::unique_ptr<Expr> cond, std::unique_ptr<Block> block,
         std::unique_ptr<Stmt> els = std::unique_ptr<Stmt>())
      : pos(pos),
        cond(std::move(cond)),
        block(std::move(block)),
        els(std::move(els)) {}

  Pos GetPos() override { return pos; }
};

struct FnArg : public Node {
  Kind NodeKind() override { return Kind::FN_ARG; }

  std::unique_ptr<Ident> ty;
  std::unique_ptr<Ident> name;

  bool withName() { return name != nullptr; }

  FnArg(std::unique_ptr<Ident> ty,
        std::unique_ptr<Ident> name = std::unique_ptr<Ident>())
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
          std::vector<std::unique_ptr<FnArg>> &&args,
          std::unique_ptr<Ident> ret = std::unique_ptr<Ident>())
      : pos(pos),
        name(std::move(name)),
        args(std::move(args)),
        ret(std::move(ret)) {}

  Pos GetPos() override { return pos; }
};

struct FnDecl : public Node {
  NodeId id;
  Kind NodeKind() override { return Kind::FN_DECL; }

  std::unique_ptr<FnProto> proto;
  std::unique_ptr<Block> block;

  explicit FnDecl(NodeId id, std::unique_ptr<FnProto> proto,
                  std::unique_ptr<Block> block)
      : id(id), proto(std::move(proto)), block(std::move(block)) {}

  Pos GetPos() override { return proto->pos; }
};

struct Extern : public Node {
  NodeId id;
  Kind NodeKind() override { return Kind::EXTERN; }
  std::unique_ptr<FnProto> proto;
  Pos pos;

  explicit Extern(NodeId id, Pos pos, std::unique_ptr<FnProto> proto)
      : id(id), pos(pos), proto(std::move(proto)) {}

  Pos GetPos() override { return pos; }
};

struct File {
  std::vector<std::unique_ptr<Extern>> externs;
  std::vector<std::unique_ptr<FnDecl>> fnDecls;
};

}  // namespace felis

#endif  // FELIS_SYNTAX_AST_H_
