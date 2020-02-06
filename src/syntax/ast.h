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

class Node {
 public:
  enum Kind { EXTERN, FN_DECL, FN_PROTO, FN_ARG, STMT };
  virtual Kind nodeKind() = 0;
};

class Stmt : public Node {
 public:
  enum Kind { EXPR, RET, VAR_DECL, ASSIGN, IF, BLOCK };
  virtual Kind stmtKind() = 0;
  Node::Kind nodeKind() { return Node::Kind::STMT; }
};

class Block : public Stmt {
 public:
  Kind stmtKind() { return Stmt::Kind::BLOCK; }
  std::vector<std::unique_ptr<Stmt>> stmts;
};

class Expr : public Stmt {
 public:
  enum Kind { IDENT, BINARY, LIT, CALL, UNARY };
  virtual Kind exprKind() = 0;
  Stmt::Kind stmtKind() { return Stmt::Kind::EXPR; }
};

class Ident : public Expr {
 public:
  Expr::Kind exprKind() { return Expr::Kind::IDENT; }

  std::string sval;

  Ident(std::string sval = "") : sval(sval) {}
};

class Lit : public Expr {
 public:
  enum Kind { INT, BOOL, CHAR, STR };
  virtual Kind litKind() = 0;
  Expr::Kind exprKind() { return Expr::Kind::LIT; }
};

class LitInt : public Lit {
 public:
  Kind litKind() { return Kind::INT; }

  uint64_t ival;

  LitInt(uint64_t ival = 0) : ival(ival) {}
};

class LitBool : public Lit {
 public:
  Kind litKind() { return Kind::BOOL; }

  bool bval;

  LitBool(bool bval = false) : bval(bval) {}
};

class LitStr : public Lit {
 public:
  Kind litKind() { return Kind::STR; }

  std::string sval;

  LitStr(std::string sval) : sval(sval) {}
};

class LitChar : public Lit {
 public:
  Kind litKind() { return Kind::CHAR; }

  char cval;

  LitChar(char cval = 0) : cval(cval) {}
};

enum UnOp { NEG, NOT };

class BinaryExpr : public Expr {
 public:
  Expr::Kind exprKind() { return Expr::Kind::BINARY; }

  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  BinOp op;

  BinaryExpr(std::unique_ptr<Expr> lhs, BinOp op, std::unique_ptr<Expr> rhs)
      : lhs(move(lhs)), rhs(move(rhs)), op(op) {}
};

class CallExpr : public Expr {
 public:
  Expr::Kind exprKind() { return Expr::Kind::CALL; }

  std::unique_ptr<Ident> ident;
  std::vector<std::unique_ptr<Expr>> args;
  CallExpr(std::unique_ptr<Ident> ident,
           std::vector<std::unique_ptr<Expr>> args)
      : ident(move(ident)), args(move(args)) {}
};

class UnaryExpr : public Expr {
 public:
  Expr::Kind exprKind() { return Expr::Kind::UNARY; }

  std::unique_ptr<UnOp> unOp;
  std::unique_ptr<Expr> expr;
  UnaryExpr(std::unique_ptr<UnOp> unOp, std::unique_ptr<Expr> expr)
      : unOp(move(unOp)), expr(move(expr)) {}
};

class RetStmt : public Stmt {
 public:
  Kind stmtKind() { return Stmt::Kind::RET; }
  std::unique_ptr<Expr> expr;

  RetStmt(std::unique_ptr<Expr> expr = std::unique_ptr<Expr>())
      : expr(move(expr)) {}
};

class VarDeclStmt : public Stmt {
 public:
  Kind stmtKind() { return Stmt::Kind::VAR_DECL; }
  bool isLet = false;
  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  VarDeclStmt(bool isLet, std::unique_ptr<Ident> name,
              std::unique_ptr<Expr> expr)
      : isLet(isLet), name(move(name)), expr(move(expr)) {}
};

class AssignStmt : public Stmt {
 public:
  Kind stmtKind() { return Stmt::Kind::ASSIGN; }
  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  AssignStmt(std::unique_ptr<Ident> name, std::unique_ptr<Expr> expr)
      : name(move(name)), expr(move(expr)) {}
};

class IfStmt : public Stmt {
 public:
  Kind stmtKind() { return Stmt::Kind::IF; }
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Stmt> els;  // block or if

  IfStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Block> block,
         std::unique_ptr<Stmt> els = std::unique_ptr<Stmt>())
      : cond(move(cond)), block(move(block)), els(move(els)) {}
};

class FnArg : public Node {
 public:
  Kind nodeKind() { return Kind::FN_ARG; }
  std::unique_ptr<Ident> ty;
  std::unique_ptr<Ident> name;

  bool withName() { return name != nullptr; }

  FnArg(std::unique_ptr<Ident> ty,
        std::unique_ptr<Ident> name = std::unique_ptr<Ident>())
      : ty(move(ty)), name(move(name)) {}
};

class FnProto : public Node {
 public:
  Kind nodeKind() { return Kind::FN_PROTO; }
  std::unique_ptr<Ident> name;
  std::vector<std::unique_ptr<FnArg>> args;
  std::unique_ptr<Ident> ret;

  FnProto(std::unique_ptr<Ident> name,
          std::vector<std::unique_ptr<FnArg>> &&args,
          std::unique_ptr<Ident> ret = std::unique_ptr<Ident>())
      : name(move(name)), args(move(args)), ret(move(ret)) {}
};

class FnDecl : public Node {
 public:
  Kind nodeKind() { return Kind::FN_DECL; }
  std::unique_ptr<FnProto> proto;
  std::unique_ptr<Block> block;

  FnDecl(std::unique_ptr<FnProto> proto, std::unique_ptr<Block> block)
      : proto(move(proto)), block(move(block)) {}
};

class Extern : public Node {
 public:
  Kind nodeKind() { return Kind::EXTERN; }
  std::unique_ptr<FnProto> proto;

  Extern(std::unique_ptr<FnProto> proto) : proto(move(proto)) {}
};

class File {
 public:
  std::vector<std::unique_ptr<Extern>> externs;
  std::vector<std::unique_ptr<FnDecl>> fnDecls;
};

}  // namespace felis

#endif  // FELIS_SYNTAX_AST_H_
