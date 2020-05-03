#ifndef FELIS_CHECK_HIR_H_
#define FELIS_CHECK_HIR_H_

#include "check/decl.h"
#include "check/type.h"
#include "syntax/ast.h"
#include "syntax/rune.h"

namespace felis {

namespace hir {

struct Stmt {
  enum Kind { EXPR, RET, VAR_DECL, ASSIGN, IF, BLOCK };

  virtual Kind StmtKind() = 0;
};

struct Expr : public Stmt {
  Stmt::Kind StmtKind() override { return Stmt::Kind::EXPR; }

  enum Kind { BINARY, VALUE, CALL, UNARY };
  virtual Kind ExprKind() = 0;
  virtual std::shared_ptr<Type> Ty() = 0;
  Expr(Pos pos) : pos(pos){};
  Pos pos;

  void Debug();

  bool IsConstant();
};

struct Value : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::VALUE; }

  enum Kind { CONSTANT, VARIABLE };

  virtual Kind ValueKind() = 0;
  Value(Pos pos) : Expr(pos){};

  bool IsConstant() { return ValueKind() == Kind::CONSTANT; }
};

struct Constant : public Value {
  Value::Kind ValueKind() { return Value::Kind::CONSTANT; }

  enum Kind { INT, FLOAT, CHAR, BOOL, STRING };

  virtual Constant::Kind ConstantKind() = 0;
  Constant(Pos pos) : Value(pos){};
};

struct IntConstant : public Constant {
  IntConstant(Pos pos, int64_t val)
      : Constant(pos), val(val), is32(val <= INT32_MAX){};
  int64_t val;
  bool is32;

  Constant::Kind ConstantKind() { return Constant::Kind::INT; };

  std::shared_ptr<Type> Ty() {
    return std::make_shared<Type>(is32 ? Type::Kind::I32 : Type::Kind::I64);
  }
};

struct FloatConstant : public Constant {
  FloatConstant(Pos pos, double val) : Constant(pos), val(val){};
  double val;

  Constant::Kind ConstantKind() { return Constant::Kind::FLOAT; };

  std::shared_ptr<Type> Ty() { return std::make_shared<Type>(Type::Kind::F64); }
};

struct CharConstant : public Constant {
  CharConstant(Pos pos, rune val) : Constant(pos), val(val){};
  rune val;

  Constant::Kind ConstantKind() { return Constant::Kind::CHAR; };

  std::shared_ptr<Type> Ty() {
    return std::make_shared<Type>(Type::Kind::CHAR);
  }
};

struct BoolConstant : public Constant {
  BoolConstant(Pos pos, bool val) : Constant(pos), val(val){};
  bool val;

  Constant::Kind ConstantKind() { return Constant::Kind::BOOL; };

  std::shared_ptr<Type> Ty() {
    return std::make_shared<Type>(Type::Kind::BOOL);
  }
};

struct StringConstant : public Constant {
  StringConstant(Pos pos, std::string val) : Constant(pos), val(val){};
  std::string val;

  Constant::Kind ConstantKind() { return Constant::Kind::STRING; };

  std::shared_ptr<Type> Ty() {
    return std::make_shared<Type>(Type::Kind::STRING);
  }
};

struct Call : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::CALL; }

  Call(Pos pos) : Expr(pos) {}
  std::shared_ptr<Decl> decl;
  std::vector<std::unique_ptr<Expr>> argExprs;

  std::shared_ptr<Type> Ty() {
    auto fnType = (FuncType *)decl->type.get();
    return fnType->ret;
  }
};

struct Variable : public Value {
  Value::Kind ValueKind() { return Value::Kind::VARIABLE; }
  Variable(Pos pos) : Value(pos){};
  std::shared_ptr<Decl> decl;

  std::shared_ptr<Type> Ty() { return decl->type; }
};

struct Unary : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::UNARY; }

  Unary(Pos pos, ast::UnOp unOp, std::unique_ptr<Expr> exp)
      : Expr(pos), unOp(unOp), exp(std::move(exp)) {}

  ast::UnOp unOp;
  std::unique_ptr<Expr> exp;

  std::shared_ptr<Type> Ty() { return exp->Ty(); }
};

struct Binary : public Expr {
  Expr::Kind ExprKind() { return Expr::Kind::BINARY; }

  Binary(Pos pos, ast::BinOp binOp, std::unique_ptr<Expr> lhs,
         std::unique_ptr<Expr> rhs)
      : Expr(pos), binOp(binOp), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  ast::BinOp binOp;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  std::shared_ptr<Type> Ty() {
    switch (binOp) {
      case ast::BinOp::GE:
      case ast::BinOp::GT:
      case ast::BinOp::LE:
      case ast::BinOp::LT:
        return std::make_shared<Type>(Type::Kind::BOOL);
      default:
        // TODO
        return lhs->Ty();
    }
  }
};

struct Block : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::BLOCK; }

  std::vector<std::unique_ptr<Stmt>> stmts;
};

struct RetStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::RET; }

  RetStmt(std::unique_ptr<Expr> &&expr = nullptr) : expr(std::move(expr)) {}

  std::unique_ptr<Expr> expr;
};

struct VarDeclStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::VAR_DECL; }

  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  VarDeclStmt(std::shared_ptr<Decl> decl, std::unique_ptr<Expr> &&expr)
      : decl(decl), expr(std::move(expr)) {}
};

struct AssignStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::ASSIGN; }

  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  AssignStmt(std::shared_ptr<Decl> decl, std::unique_ptr<Expr> expr)
      : decl(decl), expr(std::move(expr)) {}
};

struct IfStmt : public Stmt {
  Kind StmtKind() override { return Stmt::Kind::IF; }

  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Stmt> els;  // block or if

  IfStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Block> block,
         std::unique_ptr<Stmt> els = nullptr)
      : cond(std::move(cond)), block(std::move(block)), els(std::move(els)) {}
};

struct FnDecl {
  std::shared_ptr<Decl> decl;
  std::vector<std::unique_ptr<Stmt>> stmts;

  explicit FnDecl(std::shared_ptr<Decl> decl) : decl(decl) {}
};

struct Extern {
  std::shared_ptr<Decl> decl;

  explicit Extern(std::shared_ptr<Decl> decl) : decl(decl) {}
};

struct File {
  std::vector<std::unique_ptr<Extern>> externs;
  std::vector<std::unique_ptr<FnDecl>> fnDecls;
};

}  // namespace hir

}  // namespace felis

#endif  // FELIS_CHECK_HIR_H_
