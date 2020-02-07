#ifndef FELIS_SYNTAX_AST_H_
#define FELIS_SYNTAX_AST_H_

#include <memory>
#include <string>
#include <vector>

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

struct Node {
  enum Kind { EXTERN, FN_DECL, FN_PROTO, FN_ARG, STMT };
  virtual Kind NodeKind() = 0;
};

struct Stmt : public Node {
  Node::Kind NodeKind() { return Node::Kind::STMT; }

  enum Kind { EXPR, RET, VAR_DECL, ASSIGN, IF, BLOCK };

  virtual Kind StmtKind() = 0;
};

struct Block : public Stmt {
  Kind StmtKind() { return Stmt::Kind::BLOCK; }

  std::vector<std::unique_ptr<Stmt>> stmts;
};

struct Expr : public Stmt {
  Stmt::Kind StmtKind() { return Stmt::Kind::EXPR; }

  enum Kind { IDENT, BINARY, LIT, CALL, UNARY };

  virtual Kind ExprKind() = 0;
};

struct Ident : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::IDENT; }

  std::string sval;

  explicit Ident(std::string sval = "") : sval(sval) {}
};

struct Lit : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::LIT; }

  enum Kind { INT, BOOL, CHAR, STR };

  virtual Kind LitKind() = 0;
};

struct LitInt : public Lit {
  Kind LitKind() { return Kind::INT; }

  uint64_t ival;

  explicit LitInt(uint64_t ival = 0) : ival(ival) {}
};

struct LitBool : public Lit {
  Kind LitKind() { return Kind::BOOL; }

  bool bval;

  explicit LitBool(bool bval = false) : bval(bval) {}
};

struct LitStr : public Lit {
  Kind LitKind() { return Kind::STR; }

  std::string sval;

  explicit LitStr(std::string sval) : sval(sval) {}
};

struct LitChar : public Lit {
  Kind LitKind() { return Kind::CHAR; }

  char cval;

  explicit LitChar(char cval = 0) : cval(cval) {}
};

enum UnOp { NEG, NOT };

struct BinaryExpr : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::BINARY; }

  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  BinOp op;

  BinaryExpr(std::unique_ptr<Expr> lhs, BinOp op, std::unique_ptr<Expr> rhs)
      : lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}
};

struct CallExpr : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::CALL; }

  std::unique_ptr<Ident> ident;
  std::vector<std::unique_ptr<Expr>> args;

  CallExpr(std::unique_ptr<Ident> ident,
           std::vector<std::unique_ptr<Expr>> args)
      : ident(std::move(ident)), args(std::move(args)) {}
};

struct UnaryExpr : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::UNARY; }

  std::unique_ptr<UnOp> unOp;
  std::unique_ptr<Expr> expr;

  UnaryExpr(std::unique_ptr<UnOp> unOp, std::unique_ptr<Expr> expr)
      : unOp(std::move(unOp)), expr(std::move(expr)) {}
};

struct RetStmt : public Stmt {
  Kind StmtKind() { return Stmt::Kind::RET; }

  std::unique_ptr<Expr> expr;

  explicit RetStmt(std::unique_ptr<Expr> expr = std::unique_ptr<Expr>())
      : expr(std::move(expr)) {}
};

struct VarDeclStmt : public Stmt {
  Kind StmtKind() { return Stmt::Kind::VAR_DECL; }

  bool isLet;
  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  VarDeclStmt(bool isLet, std::unique_ptr<Ident> name,
              std::unique_ptr<Expr> expr)
      : isLet(isLet), name(std::move(name)), expr(std::move(expr)) {}
};

struct AssignStmt : public Stmt {
  Kind StmtKind() { return Stmt::Kind::ASSIGN; }

  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  AssignStmt(std::unique_ptr<Ident> name, std::unique_ptr<Expr> expr)
      : name(std::move(name)), expr(std::move(expr)) {}
};

struct IfStmt : public Stmt {
  Kind StmtKind() { return Stmt::Kind::IF; }

  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Stmt> els;  // block or if

  IfStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Block> block,
         std::unique_ptr<Stmt> els = std::unique_ptr<Stmt>())
      : cond(std::move(cond)), block(std::move(block)), els(std::move(els)) {}
};

struct FnArg : public Node {
  Kind NodeKind() { return Kind::FN_ARG; }

  std::unique_ptr<Ident> ty;
  std::unique_ptr<Ident> name;

  bool withName() { return name != nullptr; }

  FnArg(std::unique_ptr<Ident> ty,
        std::unique_ptr<Ident> name = std::unique_ptr<Ident>())
      : ty(std::move(ty)), name(std::move(name)) {}
};

struct FnProto : public Node {
  Kind NodeKind() { return Kind::FN_PROTO; }

  std::unique_ptr<Ident> name;
  std::vector<std::unique_ptr<FnArg>> args;
  std::unique_ptr<Ident> ret;

  FnProto(std::unique_ptr<Ident> name,
          std::vector<std::unique_ptr<FnArg>> &&args,
          std::unique_ptr<Ident> ret = std::unique_ptr<Ident>())
      : name(std::move(name)), args(std::move(args)), ret(std::move(ret)) {}
};

struct FnDecl : public Node {
  Kind NodeKind() { return Kind::FN_DECL; }

  std::unique_ptr<FnProto> proto;
  std::unique_ptr<Block> block;

  FnDecl(std::unique_ptr<FnProto> proto, std::unique_ptr<Block> block)
      : proto(std::move(proto)), block(std::move(block)) {}
};

struct Extern : public Node {
  Kind NodeKind() { return Kind::EXTERN; }
  std::unique_ptr<FnProto> proto;

  explicit Extern(std::unique_ptr<FnProto> proto) : proto(std::move(proto)) {}
};

struct File {
  std::vector<std::unique_ptr<Extern>> externs;
  std::vector<std::unique_ptr<FnDecl>> fnDecls;
};

}  // namespace felis

#endif  // FELIS_SYNTAX_AST_H_
