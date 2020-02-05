#ifndef AST_HPP
#define AST_HPP

#include <memory>
#include <string>
#include <vector>

using namespace std;

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
  Node::Kind nodeKind() { return Node::Kind::STMT; };
};

class Block : public Stmt {
 public:
  Kind stmtKind() { return Stmt::Kind::BLOCK; };
  vector<unique_ptr<Stmt>> stmts;
};

class Expr : public Stmt {
 public:
  enum Kind { IDENT, BINARY, LIT };
  virtual Kind exprKind() = 0;
  Stmt::Kind stmtKind() { return Stmt::Kind::EXPR; };
};

class Ident : public Expr {
 public:
  Expr::Kind exprKind() { return Expr::Kind::IDENT; };

  string sval;

  Ident(string sval = "") : sval(sval){};
};

class Lit : public Expr {
 public:
  enum Kind { INT, BOOL, CHAR, STR };
  virtual Kind litKind() = 0;
  Expr::Kind exprKind() { return Expr::Kind::LIT; };
};

class LitInt : public Lit {
 public:
  Kind litKind() { return Kind::INT; };

  uint64_t ival;

  LitInt(uint64_t ival = 0) : ival(ival){};
};

class LitBool : public Lit {
 public:
  Kind litKind() { return Kind::BOOL; };

  bool bval;

  LitBool(bool bval = false) : bval(bval){};
};

class LitStr : public Lit {
 public:
  Kind litKind() { return Kind::STR; };

  string sval;

  LitStr(string sval) : sval(sval){};
};

class LitChar : public Lit {
 public:
  Kind litKind() { return Kind::CHAR; };

  char cval;

  LitChar(char cval = 0) : cval(cval){};
};

enum UnOp { NEG, NOT };

class BinaryExpr : public Expr {
 public:
  Expr::Kind exprKind() { return Expr::Kind::BINARY; };

  unique_ptr<Expr> lhs;
  unique_ptr<Expr> rhs;
  BinOp op;

  BinaryExpr(unique_ptr<Expr> lhs, BinOp op, unique_ptr<Expr> rhs)
      : lhs(move(lhs)), rhs(move(rhs)), op(op){};
};

class RetStmt : public Stmt {
 public:
  Kind stmtKind() { return Stmt::Kind::RET; };
  unique_ptr<Expr> expr;

  RetStmt(unique_ptr<Expr> expr = unique_ptr<Expr>()) : expr(move(expr)){};
};

class VarDeclStmt : public Stmt {
 public:
  Kind stmtKind() { return Stmt::Kind::VAR_DECL; };
  bool isLet = false;
  unique_ptr<Ident> name;
  unique_ptr<Expr> expr;

  VarDeclStmt(bool isLet, unique_ptr<Ident> name, unique_ptr<Expr> expr)
      : isLet(isLet), name(move(name)), expr(move(expr)){};
};

class AssignStmt : public Stmt {
 public:
  Kind stmtKind() { return Stmt::Kind::ASSIGN; };
  unique_ptr<Ident> name;
  unique_ptr<Expr> expr;

  AssignStmt(unique_ptr<Ident> name, unique_ptr<Expr> expr)
      : name(move(name)), expr(move(expr)){};
};

class IfStmt : public Stmt {
 public:
  Kind stmtKind() { return Stmt::Kind::IF; };
  unique_ptr<Expr> cond;
  unique_ptr<Block> block;
  unique_ptr<Stmt> els;  // block or if

  IfStmt(unique_ptr<Expr> cond, unique_ptr<Block> block,
         unique_ptr<Stmt> els = unique_ptr<Stmt>())
      : cond(move(cond)), block(move(block)), els(move(els)){};
};

class FnArg : public Node {
 public:
  Kind nodeKind() { return Kind::FN_ARG; };
  unique_ptr<Ident> ty;
  unique_ptr<Ident> name;

  bool withName() { return name != nullptr; };

  FnArg(unique_ptr<Ident> ty, unique_ptr<Ident> name = unique_ptr<Ident>())
      : ty(move(ty)), name(move(name)){};
};

class FnProto : public Node {
 public:
  Kind nodeKind() { return Kind::FN_PROTO; };
  unique_ptr<Ident> name;
  vector<unique_ptr<FnArg>> args;
  unique_ptr<Ident> ret;

  FnProto(unique_ptr<Ident> name, vector<unique_ptr<FnArg>> &&args,
          unique_ptr<Ident> ret = unique_ptr<Ident>())
      : name(move(name)), args(move(args)), ret(move(ret)){};
};

class FnDecl : public Node {
 public:
  Kind nodeKind() { return Kind::FN_DECL; };
  unique_ptr<FnProto> proto;
  unique_ptr<Block> block;

  FnDecl(unique_ptr<FnProto> proto, unique_ptr<Block> block)
      : proto(move(proto)), block(move(block)){};
};

class Extern : public Node {
 public:
  Kind nodeKind() { return Kind::EXTERN; };
  unique_ptr<FnProto> proto;

  Extern(unique_ptr<FnProto> proto) : proto(move(proto)){};
};

class File {
 public:
  vector<unique_ptr<Extern>> externs;
  vector<unique_ptr<FnDecl>> fnDecls;
};

#endif
