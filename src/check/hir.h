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
  virtual bool IsTerminating() { return false; }

  bool IsRet() { return StmtKind() == Kind::RET; }
};

struct Expr : public Stmt {
  Stmt::Kind StmtKind() override { return Stmt::Kind::EXPR; }

  enum Kind { BINARY, VALUE, CALL, UNARY };
  virtual Expr::Kind ExprKind() = 0;
  virtual std::shared_ptr<Type> Ty() = 0;
  virtual bool IsConstant() = 0;

  Expr(Pos pos) : pos(pos){};

  Pos pos;

  void Debug();
};

struct Value : public Expr {
  enum Kind { CONSTANT, VARIABLE };
  virtual Value::Kind ValueKind() = 0;

  Value(Pos pos) : Expr(pos){};

  Expr::Kind ExprKind() override { return Expr::Kind::VALUE; }

  bool IsConstant() override { return ValueKind() == Kind::CONSTANT; }
};

struct Constant : public Value {
  enum Kind { INT, FLOAT, CHAR, BOOL, STRING };
  virtual Constant::Kind ConstantKind() = 0;

  Constant(Pos pos) : Value(pos){};

  Value::Kind ValueKind() override { return Value::Kind::CONSTANT; }
};

struct IntConstant : public Constant {
  IntConstant(Pos pos, int64_t val)
      : Constant(pos), val(val), is32(val <= INT32_MAX){};

  int64_t val;
  bool is32;

  std::shared_ptr<Type> Ty() override {
    return std::make_shared<Type>(is32 ? Type::Kind::I32 : Type::Kind::I64);
  }

  Constant::Kind ConstantKind() override { return Constant::Kind::INT; };
};

struct FloatConstant : public Constant {
  FloatConstant(Pos pos, double val, bool is32)
      : Constant(pos), val(val), is32(is32){};

  double val;
  bool is32;

  std::shared_ptr<Type> Ty() override {
    return std::make_shared<Type>(is32 ? Type::Kind::F32 : Type::Kind::F64);
  }

  Constant::Kind ConstantKind() override { return Constant::Kind::FLOAT; };
};

struct CharConstant : public Constant {
  CharConstant(Pos pos, rune val) : Constant(pos), val(val){};

  rune val;

  std::shared_ptr<Type> Ty() override {
    return std::make_shared<Type>(Type::Kind::CHAR);
  }

  Constant::Kind ConstantKind() override { return Constant::Kind::CHAR; };
};

struct BoolConstant : public Constant {
  BoolConstant(Pos pos, bool val) : Constant(pos), val(val){};

  bool val;

  std::shared_ptr<Type> Ty() override {
    return std::make_shared<Type>(Type::Kind::BOOL);
  }

  Constant::Kind ConstantKind() override { return Constant::Kind::BOOL; };
};

struct StringConstant : public Constant {
  StringConstant(Pos pos, std::string val) : Constant(pos), val(val){};

  std::string val;

  std::shared_ptr<Type> Ty() override {
    return std::make_shared<Type>(Type::Kind::STRING);
  }

  Constant::Kind ConstantKind() override { return Constant::Kind::STRING; };
};

struct Call : public Expr {
  Call(Pos pos) : Expr(pos) {}

  std::shared_ptr<Decl> decl;
  std::deque<std::unique_ptr<Expr>> args;

  std::shared_ptr<Type> Ty() override {
    auto fnType = (FuncType*)decl->type.get();
    return fnType->ret;
  }

  Expr::Kind ExprKind() override { return Expr::Kind::CALL; }

  bool IsConstant() override { return false; }
};

struct Variable : public Value {
  Variable(Pos pos) : Value(pos){};

  std::shared_ptr<Decl> decl;

  std::shared_ptr<Type> Ty() override { return decl->type; }

  Value::Kind ValueKind() override { return Value::Kind::VARIABLE; }
};

struct Unary : public Expr {
  Unary(Pos pos, ast::UnOp unOp, std::unique_ptr<Expr> expr)
      : Expr(pos), unOp(unOp), expr(std::move(expr)) {}

  ast::UnOp unOp;
  std::unique_ptr<Expr> expr;

  std::shared_ptr<Type> Ty() override { return expr->Ty(); }

  Expr::Kind ExprKind() override { return Expr::Kind::UNARY; }

  bool IsConstant() override { return false; }
};

struct Binary : public Expr {
  Binary(Pos pos, ast::BinOp binOp, std::unique_ptr<Expr> lhs,
         std::unique_ptr<Expr> rhs)
      : Expr(pos), binOp(binOp), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  ast::BinOp binOp;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;

  std::shared_ptr<Type> Ty() override {
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

  Expr::Kind ExprKind() override { return Expr::Kind::BINARY; }

  bool IsConstant() override { return false; }
};

struct Block : public Stmt {
  std::deque<std::unique_ptr<Stmt>> stmts;

  Kind StmtKind() override { return Stmt::Kind::BLOCK; }

  bool IsTerminating() override {
    if (stmts.empty()) return false;
    return stmts.back()->IsTerminating();
  }
};

struct RetStmt : public Stmt {
  RetStmt(std::unique_ptr<Expr> expr = nullptr) : expr(std::move(expr)) {}
  bool IsTerminating() override { return true; }

  std::unique_ptr<Expr> expr;

  Kind StmtKind() override { return Stmt::Kind::RET; }
};

struct VarDeclStmt : public Stmt {
  VarDeclStmt(std::shared_ptr<Decl> decl, std::unique_ptr<Expr> expr)
      : decl(decl), expr(std::move(expr)) {}

  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  Kind StmtKind() override { return Stmt::Kind::VAR_DECL; }
};

struct AssignStmt : public Stmt {
  AssignStmt(std::shared_ptr<Decl> decl, std::unique_ptr<Expr> expr)
      : decl(decl), expr(std::move(expr)) {}

  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  Kind StmtKind() override { return Stmt::Kind::ASSIGN; }
};

struct IfStmt : public Stmt {
  IfStmt(std::unique_ptr<Expr> cond, std::unique_ptr<Block> block,
         std::unique_ptr<Stmt> els = nullptr)
      : cond(std::move(cond)), block(std::move(block)), els(std::move(els)) {}

  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Stmt> els;  // block or if

  Kind StmtKind() override { return Stmt::Kind::IF; }

  bool IsTerminating() override {
    if (els == nullptr) return false;
    return block->IsTerminating() && els->IsTerminating();
  }
};

struct FnDecl {
  FnDecl(std::shared_ptr<Decl> decl) : decl(decl) {}

  std::shared_ptr<Decl> decl;
  std::deque<std::shared_ptr<Decl>> args;
  std::unique_ptr<Block> block;
};

struct Extern {
  Extern(std::shared_ptr<Decl> decl) : decl(decl) {}

  std::shared_ptr<Decl> decl;
};

struct File {
  std::vector<std::unique_ptr<Extern>> externs;
  std::deque<std::unique_ptr<FnDecl>> fnDecls;
};

}  // namespace hir

}  // namespace felis

#endif  // FELIS_CHECK_HIR_H_
