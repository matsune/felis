#ifndef FELIS_NODE_HIR_H_
#define FELIS_NODE_HIR_H_

#include "check/decl.h"
#include "check/type.h"
#include "macro.h"
#include "node/ast.h"
#include "node/node.h"
#include "syntax/rune.h"

namespace felis {

namespace hir {

struct Stmt : Node {
  enum Kind { EXPR, RET, VAR_DECL, ASSIGN };
  virtual Kind StmtKind() const = 0;

  bool IsRet() const { return StmtKind() == Kind::RET; }
};

struct Expr : public Stmt {
  Loc begin;
  Loc end;

  Expr(Loc begin, Loc end) : begin(begin), end(end) {}

  enum Kind { BINARY, VALUE, CALL, UNARY, IF, BLOCK };
  virtual Expr::Kind ExprKind() const = 0;
  virtual std::shared_ptr<Type> Ty() const = 0;
  virtual bool IsConstant() const { return false; }

  // override Stmt
  Stmt::Kind StmtKind() const override { return Stmt::Kind::EXPR; }

  // override Node
  Loc Begin() const override { return begin; }
  Loc End() const override { return end; }
};

struct Value : public Expr {
  Value(Loc begin, Loc end) : Expr(begin, end) {}

  enum Kind { CONSTANT, VARIABLE };
  virtual Value::Kind ValueKind() const = 0;

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::VALUE; }
  bool IsConstant() const override { return ValueKind() == Kind::CONSTANT; }
};

struct Constant : public Value {
  Constant(Loc begin, Loc end) : Value(begin, end) {}

  enum Kind { INT, FLOAT, CHAR, BOOL, STRING };
  virtual Constant::Kind ConstantKind() const = 0;

  // override Value
  Value::Kind ValueKind() const override { return Value::Kind::CONSTANT; }
};

struct IntConstant : public Constant {
  int64_t val;
  bool is_32;

  IntConstant(Loc begin, Loc end, int64_t val)
      : Constant(begin, end), val(val), is_32(val <= INT32_MAX){};

  // override Expr
  std::shared_ptr<Type> Ty() const override {
    return std::make_shared<Type>(is_32 ? Type::Kind::I32 : Type::Kind::I64);
  }

  // override Constant
  Constant::Kind ConstantKind() const override { return Constant::Kind::INT; };
};

struct FloatConstant : public Constant {
  Loc begin;
  Loc end;
  double val;
  bool is_32;

  FloatConstant(Loc begin, Loc end, double val, bool is_32)
      : Constant(begin, end), end(end), val(val), is_32(is_32){};

  // override Expr
  std::shared_ptr<Type> Ty() const override {
    return std::make_shared<Type>(is_32 ? Type::Kind::F32 : Type::Kind::F64);
  }

  // override Constant
  Constant::Kind ConstantKind() const override {
    return Constant::Kind::FLOAT;
  };
};

struct CharConstant : public Constant {
  rune val;

  CharConstant(Loc begin, Loc end, rune val) : Constant(begin, end), val(val){};

  // override Expr
  std::shared_ptr<Type> Ty() const override {
    return std::make_shared<Type>(Type::Kind::CHAR);
  }

  // override Constant
  Constant::Kind ConstantKind() const override { return Constant::Kind::CHAR; };
};

struct BoolConstant : public Constant {
  bool val;

  BoolConstant(Loc begin, Loc end, bool val) : Constant(begin, end), val(val){};

  // override Expr
  std::shared_ptr<Type> Ty() const override {
    return std::make_shared<Type>(Type::Kind::BOOL);
  }

  // override Constant
  Constant::Kind ConstantKind() const override { return Constant::Kind::BOOL; };
};

struct StringConstant : public Constant {
  std::string val;

  StringConstant(Loc begin, Loc end, std::string val)
      : Constant(begin, end), val(val){};

  // override Expr
  std::shared_ptr<Type> Ty() const override {
    return std::make_shared<Type>(Type::Kind::STRING);
  }

  // override Constant
  Constant::Kind ConstantKind() const override {
    return Constant::Kind::STRING;
  };
};

struct Call : public Expr {
  std::shared_ptr<Decl> decl;
  unique_deque<Expr> args;

  Call(Loc begin, Loc end) : Expr(begin, end) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::CALL; }
  std::shared_ptr<Type> Ty() const override {
    auto fn_type = (FuncType*)decl->type.get();
    return fn_type->ret;
  }
};

struct Variable : public Value {
  std::shared_ptr<Decl> decl;

  Variable(Loc begin, Loc end) : Value(begin, end){};

  // override Expr
  std::shared_ptr<Type> Ty() const override { return decl->type; }

  // override Value
  Value::Kind ValueKind() const override { return Value::Kind::VARIABLE; }
};

struct Unary : public Expr {
  enum Op { NEG, NOT };

  Unary::Op op;
  std::unique_ptr<Expr> expr;

  Unary(Loc begin, Loc end, Unary::Op op, std::unique_ptr<Expr> expr)
      : Expr(begin, end), op(op), expr(std::move(expr)) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::UNARY; }
  std::shared_ptr<Type> Ty() const override { return expr->Ty(); }
};

struct Binary : public Expr {
  enum Op {
    LT,
    LE,
    GT,
    GE,
    ADD,
    SUB,

    MUL,
    DIV,
    MOD
  };

  Binary::Op op;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;

  Binary(Loc begin, Loc end, Binary::Op op, std::unique_ptr<Expr> lhs,
         std::unique_ptr<Expr> rhs)
      : Expr(begin, end), op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::BINARY; }
  std::shared_ptr<Type> Ty() const override {
    switch (op) {
      case Binary::Op::GE:
      case Binary::Op::GT:
      case Binary::Op::LE:
      case Binary::Op::LT:
        return std::make_shared<Type>(Type::Kind::BOOL);
      default:
        // TODO
        return lhs->Ty();
    }
  }
};

struct Block : public Expr {
  unique_deque<Stmt> stmts;

  Block(Loc begin, Loc end, unique_deque<Stmt> stmts)
      : Expr(begin, end), stmts(std::move(stmts)) {}

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::BLOCK; }

  std::shared_ptr<Type> Ty() const override { UNIMPLEMENTED }

  /* bool IsTerminating() const { */
  /*   if (stmts.empty()) return false; */
  /*   return stmts.back()->IsTerminating(); */
  /* } */
};

struct RetStmt : public Stmt {
  Loc begin;
  std::unique_ptr<Expr> expr;

  RetStmt(Loc begin, std::unique_ptr<Expr> expr = nullptr)
      : expr(std::move(expr)) {}

  /* bool IsTerminating() const override { return true; } */

  // override Stmt
  Kind StmtKind() const override { return Stmt::Kind::RET; }

  // override Node
  Loc Begin() const override { return begin; }

  Loc End() const override {
    if (expr)
      return expr->End();
    else
      return begin + 2;
  }
};

struct VarDeclStmt : public Stmt {
  Loc begin;
  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  VarDeclStmt(Loc begin, std::shared_ptr<Decl> decl, std::unique_ptr<Expr> expr)
      : begin(begin), decl(decl), expr(std::move(expr)) {}

  // override Stmt
  Kind StmtKind() const override { return Stmt::Kind::VAR_DECL; }

  // override Node
  Loc Begin() const override { return begin; }

  Loc End() const override { return expr->End(); }
};

struct AssignStmt : public Stmt {
  Loc begin;
  std::shared_ptr<Decl> decl;
  std::unique_ptr<Expr> expr;

  AssignStmt(Loc begin, std::shared_ptr<Decl> decl, std::unique_ptr<Expr> expr)
      : begin(begin), decl(decl), expr(std::move(expr)) {}

  // override Stmt
  Kind StmtKind() const override { return Stmt::Kind::ASSIGN; }

  // override Node
  Loc Begin() const override { return begin; }

  Loc End() const override { return expr->End(); }
};

struct If : public Expr {
  /* Loc begin; */
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Expr> els;  // null or Block or If

  If(Loc begin, Loc end, std::unique_ptr<Expr> cond,
     std::unique_ptr<Block> block, std::unique_ptr<Expr> els = nullptr)
      : Expr(begin, end),
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

  // override Expr
  Expr::Kind ExprKind() const override { return Expr::Kind::IF; }
  std::shared_ptr<Type> Ty() const override { UNIMPLEMENTED }

  /* bool IsTerminating() const override { */
  /*   if (els == nullptr) return false; */
  /*   return block->IsTerminating() && els->IsTerminating(); */
  /* } */

  /* Loc Begin() const override { return begin; } */

  /* Loc End() const override { */
  /*   if (els) */
  /*     return els->End(); */
  /*   else */
  /*     return block->End(); */
  /* } */
};

struct FnDecl : public Node {
  Loc begin;
  std::shared_ptr<Decl> decl;
  std::deque<std::shared_ptr<Decl>> args;
  std::unique_ptr<Block> block;

  FnDecl(Loc begin, std::shared_ptr<Decl> decl) : begin(begin), decl(decl) {}

  // override Node
  Loc Begin() const override { return begin; }

  Loc End() const override { return block->End(); }
};

struct Extern : public Node {
  Loc begin;
  Loc end;
  std::shared_ptr<Decl> decl;

  Extern(Loc begin, Loc end, std::shared_ptr<Decl> decl) : decl(decl) {}

  // override Node
  Loc Begin() const override { return begin; }

  Loc End() const override { return end; }
};

struct File {
  std::vector<std::unique_ptr<Extern>> externs;
  unique_deque<FnDecl> fn_decls;
};

}  // namespace hir

}  // namespace felis

#endif  // FELIS_NODE_HIR_H_
