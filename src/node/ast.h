#ifndef FELIS_NODE_AST_H_
#define FELIS_NODE_AST_H_

#include <cassert>
#include <memory>
#include <string>
#include <vector>

#include "loc.h"
#include "node/node.h"
#include "unique.h"

namespace felis {

namespace ast {

struct AstNode : public Node {
  virtual const Loc Begin() const = 0;
  virtual const Loc End() const = 0;

  template <typename T>
  T *As() {
    return dynamic_cast<T *>(this);
  }

  template <typename T>
  const T *As() const {
    return dynamic_cast<const T *>(this);
  }
};

struct Operator : public AstNode {
  enum Kind { UNARY, BINARY };
  virtual Operator::Kind OpKind() const = 0;
};

struct UnaryOp : public Operator {
  enum Kind { NEG, NOT };

  const Loc begin;
  const UnaryOp::Kind kind;

  UnaryOp(Loc begin, UnaryOp::Kind kind) : begin(begin), kind(kind) {}

  Operator::Kind OpKind() const override { return Operator::Kind::UNARY; }

  const Loc Begin() const override { return begin; }

  const Loc End() const override { return begin; }
};

struct BinaryOp : public Operator {
  enum Op {
    EQEQ = 1,
    NEQ = 2,
    LT = 3,
    LE = 4,
    GT = 5,
    GE = 6,

    ADD = 11,
    SUB = 12,

    MUL = 21,
    DIV = 22,
    MOD = 23
  };

  const BinaryOp::Op op;
  const Loc begin;

  BinaryOp(Loc begin, BinaryOp::Op op) : begin(begin), op(op) {}

  Operator::Kind OpKind() const override { return Operator::Kind::BINARY; }

  const Loc Begin() const override { return begin; }

  const Loc End() const override {
    switch (op) {
      case BinaryOp::Op::EQEQ:
      case BinaryOp::Op::NEQ:
      case BinaryOp::Op::LE:
      case BinaryOp::Op::GE:
        return begin + 1;
      case BinaryOp::Op::LT:
      case BinaryOp::Op::GT:
      case BinaryOp::Op::ADD:
      case BinaryOp::Op::SUB:
      case BinaryOp::Op::MUL:
      case BinaryOp::Op::DIV:
      case BinaryOp::Op::MOD:
        return begin;
    }
  }
};

struct Stmt : public AstNode {
  enum Kind { EXPR, RET, VAR_DECL, ASSIGN };
  virtual Stmt::Kind StmtKind() const = 0;
};

struct Expr : public Stmt {
  enum Kind { IDENT, BINARY, LIT, CALL, UNARY, BLOCK, IF, ARRAY };
  virtual Expr::Kind ExprKind() const = 0;

  Stmt::Kind StmtKind() const override { return Stmt::Kind::EXPR; }
};

struct Block : public Expr {
  const Loc begin;
  const Loc end;
  unique_deque<Stmt> stmts;

  Block(Loc begin, Loc end, unique_deque<Stmt> stmts)
      : begin(begin), end(end), stmts(std::move(stmts)) {}

  Expr::Kind ExprKind() const override { return Expr::Kind::BLOCK; }

  const Loc Begin() const override { return begin; }

  const Loc End() const override { return end; }
};

struct If : public Expr {
  Loc begin;
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Block> block;
  std::unique_ptr<Expr> els;  // block or if

  If(Loc begin, std::unique_ptr<Expr> cond, std::unique_ptr<Block> block,
     std::unique_ptr<Expr> els = nullptr)
      : begin(begin),
        cond(std::move(cond)),
        block(std::move(block)),
        els(std::move(els)) {}

  inline bool HasElse() const { return els != nullptr; }

  inline bool IsElseIf() const {
    assert(HasElse());
    if (els)
      return els->ExprKind() == Expr::Kind::IF;
    else
      return false;
  }

  Expr::Kind ExprKind() const override { return Expr::Kind::IF; }

  const Loc Begin() const override { return begin; }

  const Loc End() const override {
    if (els) {
      return els->End();
    } else {
      return block->End();
    }
  }
};

struct Ident : public Expr {
  const Loc begin;
  std::string val;

  Ident(Loc begin, std::string val) : begin(begin), val(val) {}

  const Loc Begin() const override { return begin; }

  const Loc End() const override { return begin + val.size(); }

  Expr::Kind ExprKind() const override { return Expr::Kind::IDENT; }
};

struct Lit : public Expr {
  enum Kind { INT, FLOAT, CHAR, BOOL, STRING };
  const Loc begin;
  const Lit::Kind kind;
  std::string val;

  Lit(Loc begin, Lit::Kind kind, std::string val = "")
      : begin(begin), kind(kind), val(val) {}

  Lit::Kind LitKind() { return kind; }

  const Loc Begin() const override { return begin; }

  const Loc End() const override { return begin + val.size() + 2; }

  Expr::Kind ExprKind() const override { return Expr::Kind::LIT; }
};

struct Type : public AstNode {
  enum Kind { IDENT, ARRAY };
  virtual Type::Kind TypeKind() const = 0;
};

struct TypeIdent : public Type {
  const Loc begin;
  std::string val;

  TypeIdent(Loc begin, std::string val) : begin(begin), val(val) {}

  Type::Kind TypeKind() const override { return Type::Kind::IDENT; }

  const Loc Begin() const override { return begin; }

  const Loc End() const override { return begin + val.size(); }
};

// [T, n]
struct ArrayType : public Type {
  const Loc begin;
  const Loc end;
  std::unique_ptr<Type> elem;
  std::unique_ptr<Lit> size_lit;

  ArrayType(Loc begin, Loc end, std::unique_ptr<Type> elem,
            std::unique_ptr<Lit> size_lit)
      : begin(begin),
        end(end),
        elem(std::move(elem)),
        size_lit(std::move(size_lit)) {}

  const Loc Begin() const override { return begin; }

  const Loc End() const override { return end; }

  Type::Kind TypeKind() const override { return Type::Kind::ARRAY; }
};

struct BinaryExpr : public Expr {
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  std::unique_ptr<BinaryOp> op;

  BinaryExpr(std::unique_ptr<Expr> lhs, std::unique_ptr<BinaryOp> op,
             std::unique_ptr<Expr> rhs)
      : lhs(std::move(lhs)), rhs(std::move(rhs)), op(std::move(op)) {}

  const Loc Begin() const override { return lhs->Begin(); }

  const Loc End() const override { return rhs->End(); }

  Expr::Kind ExprKind() const override { return Expr::Kind::BINARY; }
};

struct CallExpr : public Expr {
  const Loc end;
  std::unique_ptr<Ident> ident;
  unique_deque<Expr> args;

  CallExpr(Loc end, std::unique_ptr<Ident> ident, unique_deque<Expr> args)
      : end(end), ident(std::move(ident)), args(std::move(args)) {}

  const Loc Begin() const override { return ident->Begin(); }

  const Loc End() const override { return end; }

  Expr::Kind ExprKind() const override { return Expr::Kind::CALL; }
};

struct UnaryExpr : public Expr {
  std::unique_ptr<UnaryOp> op;
  std::unique_ptr<Expr> expr;

  UnaryExpr(std::unique_ptr<UnaryOp> op, std::unique_ptr<Expr> expr)
      : op(std::move(op)), expr(std::move(expr)) {}

  const Loc Begin() const override { return op->Begin(); }

  const Loc End() const override { return expr->End(); }

  Expr::Kind ExprKind() const override { return Expr::Kind::UNARY; }
};

struct ArrayExpr : public Expr {
  Loc begin, end;
  unique_deque<Expr> exprs;

  ArrayExpr(Loc begin, Loc end, unique_deque<Expr> exprs)
      : begin(begin), end(end), exprs(std::move(exprs)) {}

  const Loc Begin() const override { return begin; }

  const Loc End() const override { return end; }

  Expr::Kind ExprKind() const override { return Expr::Kind::ARRAY; }
};

struct RetStmt : public Stmt {
  const Loc begin;
  const Loc end;
  std::unique_ptr<Expr> expr;  // nullable

  RetStmt(Loc begin, Loc end, std::unique_ptr<Expr> expr = nullptr)
      : begin(begin), end(end), expr(std::move(expr)) {}

  const Loc Begin() const override { return begin; }

  const Loc End() const override { return end; }

  Stmt::Kind StmtKind() const override { return Stmt::Kind::RET; }
};

// [var|let] <name> (':' <type_name>)? = <expr>
struct VarDeclStmt : public Stmt {
  const Loc begin;
  const bool is_let;
  std::unique_ptr<Ident> name;
  std::unique_ptr<Type> type_name;  // nullable
  std::unique_ptr<Expr> expr;

  VarDeclStmt(Loc begin, bool is_let, std::unique_ptr<Ident> name,
              std::unique_ptr<Type> type_name, std::unique_ptr<Expr> expr)
      : begin(begin),
        is_let(is_let),
        name(std::move(name)),
        type_name(std::move(type_name)),
        expr(std::move(expr)) {}

  const Loc Begin() const override { return begin; }

  const Loc End() const override { return expr->End(); }

  Stmt::Kind StmtKind() const override { return Stmt::Kind::VAR_DECL; }
};

struct AssignStmt : public Stmt {
  std::unique_ptr<Ident> name;
  std::unique_ptr<Expr> expr;

  AssignStmt(std::unique_ptr<Ident> name, std::unique_ptr<Expr> expr)
      : name(std::move(name)), expr(std::move(expr)) {}

  const Loc Begin() const override { return name->Begin(); }

  const Loc End() const override { return expr->End(); }

  Stmt::Kind StmtKind() const override { return Stmt::Kind::ASSIGN; }
};

// (<name> ':')? <type_name>
struct FnArg : public AstNode {
  std::unique_ptr<Type> type_name;
  std::unique_ptr<Ident> name;  // nullable

  FnArg(std::unique_ptr<Type> type_name, std::unique_ptr<Ident> name = nullptr)
      : type_name(std::move(type_name)), name(std::move(name)) {}

  bool WithName() const { return name != nullptr; }

  const Loc Begin() const override {
    return WithName() ? name->Begin() : type_name->Begin();
  }

  const Loc End() const override { return type_name->End(); }
};

struct FnArgs : public AstNode {
  const Loc begin;
  const Loc end;
  std::vector<std::unique_ptr<FnArg>> list;

  FnArgs(Loc begin, Loc end, std::vector<std::unique_ptr<FnArg>> list)
      : begin(begin), end(end), list(std::move(list)) {}

  const Loc Begin() const override { return begin; }

  const Loc End() const override { return end; }
};

// <name> '(' <args> ')' ('->' <ret>)?
struct FnProto : public AstNode {
  const Loc begin;
  std::unique_ptr<Ident> name;
  std::unique_ptr<FnArgs> args;
  std::unique_ptr<Type> ret;  // nullable

  FnProto(Loc begin, std::unique_ptr<Ident> name, std::unique_ptr<FnArgs> args,
          std::unique_ptr<Type> ret = nullptr)
      : begin(begin),
        name(std::move(name)),
        args(std::move(args)),
        ret(std::move(ret)) {}

  const Loc Begin() const override { return begin; }

  const Loc End() const override {
    if (ret)
      return ret->End();
    else
      return args->End();
  }
};

// 'fn' <proto> <block>
struct FnDecl : public AstNode {
  std::unique_ptr<FnProto> proto;
  std::unique_ptr<Block> block;

  FnDecl(std::unique_ptr<FnProto> proto, std::unique_ptr<Block> block)
      : proto(std::move(proto)), block(std::move(block)) {}

  const Loc Begin() const override { return proto->Begin(); }

  const Loc End() const override { return block->End(); }
};

// 'ext' <proto>
struct Extern : public AstNode {
  const Loc begin;
  std::unique_ptr<FnProto> proto;

  Extern(Loc begin, std::unique_ptr<FnProto> proto)
      : begin(begin), proto(std::move(proto)) {}

  const Loc Begin() const override { return begin; }

  const Loc End() const override { return proto->End(); }
};

struct File {
  unique_deque<Extern> externs;
  unique_deque<FnDecl> fn_decls;
};

}  // namespace ast

}  // namespace felis

#endif  // FELIS_NODE_AST_H_
