#ifndef FELIS_NODE_AST_H_
#define FELIS_NODE_AST_H_

#include <cassert>
#include <string>
#include <vector>

#include "loc.h"
#include "node/node.h"
#include "unique.h"

namespace felis {

namespace ast {

struct AstNode : public Node {
  Loc begin;
  Loc end;

  AstNode(Loc begin, Loc end) : begin(begin), end(end) {}
  virtual ~AstNode(){};
};

struct Operator : public AstNode {
  Operator(Loc begin, Loc end) : AstNode(begin, end) {}
};

struct UnaryOp : public Operator {
  enum Kind { NEG, NOT };

  UnaryOp::Kind kind;

  UnaryOp(Loc begin, Loc end, UnaryOp::Kind kind)
      : Operator(begin, end), kind(kind) {}
};

struct BinaryOp : public Operator {
  enum Kind {
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

  BinaryOp::Kind kind;

  BinaryOp(Loc begin, Loc end, BinaryOp::Kind kind)
      : Operator(begin, end), kind(kind) {}
};

struct Block : public AstNode {
  std::deque<AstNode*> stmts;

  Block(Loc begin, Loc end, std::deque<AstNode*> stmts)
      : AstNode(begin, end), stmts(stmts) {}

  ~Block() {
    for (auto e : stmts) delete e;
    stmts.clear();
  }
};

struct If : public AstNode {
  Loc begin;
  AstNode* cond;
  Block* block;
  AstNode* els;  // block or if

  If(Loc begin, Loc end, AstNode* cond, Block* block, AstNode* els = nullptr)
      : AstNode(begin, end), cond(cond), block(block), els(els) {}
  ~If() {
    delete cond;
    delete block;
    if (els) delete els;

    cond = nullptr;
    block = nullptr;
    els = nullptr;
  }

  bool HasElse() const { return els != nullptr; }

  bool IsElseIf() const {
    assert(HasElse());
    if (els)
      return node_isa<If>(els);
    else
      return false;
  }
};

struct Ident : public AstNode {
  std::string val;

  Ident(Loc begin, Loc end, std::string val) : AstNode(begin, end), val(val) {}
};

struct Literal : public AstNode {
  enum Kind { INT, FLOAT, CHAR, BOOL, STRING };
  Literal::Kind kind;
  std::string val;
  rune r;

  Literal(Loc begin, Loc end, Literal::Kind kind, std::string val)
      : AstNode(begin, end), kind(kind), val(val), r(0) {}

  Literal(Loc begin, Loc end, Literal::Kind kind, rune r)
      : AstNode(begin, end), kind(kind), val(""), r(r) {}

  static Literal* Int(Loc begin, Loc end, std::string val) {
    return new Literal(begin, end, Literal::Kind::INT, val);
  }

  static Literal* Float(Loc begin, Loc end, std::string val) {
    return new Literal(begin, end, Literal::Kind::FLOAT, val);
  }

  static Literal* Char(Loc begin, Loc end, rune r) {
    return new Literal(begin, end, Literal::Kind::CHAR, r);
  }

  static Literal* Bool(Loc begin, Loc end, std::string val) {
    return new Literal(begin, end, Literal::Kind::BOOL, val);
  }

  static Literal* String(Loc begin, Loc end, std::string val) {
    return new Literal(begin, end, Literal::Kind::STRING, val);
  }
};

// ArrayType ::= [T, size]
//
// T ::= Ident
//     | ArrayType
struct ArrayType : public AstNode {
  AstNode* elem;
  Literal* size_lit;

  ArrayType(Loc begin, Loc end, AstNode* elem, Literal* size_lit)
      : AstNode(begin, end), elem(elem), size_lit(size_lit) {
    assert(node_isa<Ident>(elem) || node_isa<ArrayType>(elem));
  }

  ~ArrayType() {
    delete elem;
    delete size_lit;

    elem = nullptr;
    size_lit = nullptr;
  }
};

struct Binary : public AstNode {
  AstNode* lhs;
  AstNode* rhs;
  BinaryOp* op;

  Binary(Loc begin, Loc end, AstNode* lhs, AstNode* rhs, BinaryOp* op)
      : AstNode(begin, end), lhs(lhs), rhs(rhs), op(op) {}

  ~Binary() {
    delete lhs;
    delete rhs;
    delete op;

    lhs = nullptr;
    rhs = nullptr;
    op = nullptr;
  }
};

struct Call : public AstNode {
  Ident* ident;
  std::deque<AstNode*> args;

  Call(Loc begin, Loc end, Ident* ident, std::deque<AstNode*> args)
      : AstNode(begin, end), ident(ident), args(args) {}

  ~Call() {
    delete ident;
    for (auto e : args) delete e;

    ident = nullptr;
    args.clear();
  }
};

struct Index : public AstNode {
  AstNode* expr;
  AstNode* idx_expr;

  Index(Loc begin, Loc end, AstNode* expr, AstNode* idx_expr)
      : AstNode(begin, end), expr(expr), idx_expr(idx_expr) {}

  ~Index() {
    delete expr;
    delete idx_expr;

    expr = nullptr;
    idx_expr = nullptr;
  }
};

struct Unary : public AstNode {
  UnaryOp* op;
  AstNode* expr;

  Unary(Loc begin, Loc end, UnaryOp* op, AstNode* expr)
      : AstNode(begin, end), op(op), expr(expr) {}

  ~Unary() {
    delete op;
    delete expr;

    op = nullptr;
    expr = nullptr;
  }
};

struct Array : public AstNode {
  std::deque<AstNode*> exprs;

  Array(Loc begin, Loc end, std::deque<AstNode*> exprs)
      : AstNode(begin, end), exprs(exprs) {}

  ~Array() {
    for (auto e : exprs) delete e;
    exprs.clear();
  }
};

struct RetStmt : public AstNode {
  AstNode* expr;  // nullable

  RetStmt(Loc begin, Loc end, AstNode* expr = nullptr)
      : AstNode(begin, end), expr(expr) {}

  ~RetStmt() {
    if (expr) delete expr;
    expr = nullptr;
  }
};

// [var|let] <name> (':' <type_name>)? = <expr>
struct VarDeclStmt : public AstNode {
  bool is_let;
  Ident* name;
  AstNode* type;  // nullable
  AstNode* expr;

  VarDeclStmt(Loc begin, Loc end, bool is_let, Ident* name, AstNode* type,
              AstNode* expr)
      : AstNode(begin, end),
        is_let(is_let),
        name(name),
        type(type),
        expr(expr) {}

  ~VarDeclStmt() {
    delete name;
    if (type) delete type;
    delete expr;

    name = nullptr;
    type = nullptr;
    expr = nullptr;
  }
};

// <assign-stmt> ::= <expr> '=' <expr>
struct AssignStmt : public AstNode {
  AstNode* left;
  AstNode* expr;

  AssignStmt(Loc begin, Loc end, AstNode* left, AstNode* expr)
      : AstNode(begin, end), left(left), expr(expr) {}

  ~AssignStmt() {
    delete left;
    delete expr;

    left = nullptr;
    expr = nullptr;
  }
};

// (<name> ':')? <type_name>
struct FnArg : public AstNode {
  Ident* name;  // nullable
  AstNode* type;

  FnArg(Loc begin, Loc end, Ident* name, AstNode* type)
      : AstNode(begin, end), name(name), type(type) {}
  ~FnArg() {
    if (name) delete name;
    delete type;

    name = nullptr;
    type = nullptr;
  }

  bool HasName() const { return name != nullptr; }
};

// <fn-args> ::= <fn-arg>*
struct FnArgs : public AstNode {
  std::vector<FnArg*> list;

  FnArgs(Loc begin, Loc end, std::vector<FnArg*> list)
      : AstNode(begin, end), list(list) {}
  ~FnArgs() {
    for (auto e : list) delete e;
    list.clear();
  }
};

// 'fn' <name> '(' <args> ')' ('->' <ret>)?
struct FnProto : public AstNode {
  Ident* name;
  FnArgs* args;
  AstNode* ret;  // nullable

  FnProto(Loc begin, Loc end, Ident* name, FnArgs* args, AstNode* ret = nullptr)
      : AstNode(begin, end), name(name), args(args), ret(ret) {}

  ~FnProto() {
    delete name;
    delete args;
    if (ret) delete ret;

    name = nullptr;
    args = nullptr;
    ret = nullptr;
  }
};

// <proto> <block>
struct Func : public AstNode {
  FnProto* proto;
  Block* block;

  Func(Loc begin, Loc end, FnProto* proto, Block* block)
      : AstNode(begin, end), proto(proto), block(block) {}

  ~Func() {
    delete proto;
    delete block;
    proto = nullptr;
    block = nullptr;
  }
};

// 'ext' <proto>
struct Extern : public AstNode {
  FnProto* proto;

  Extern(Loc begin, Loc end, FnProto* proto)
      : AstNode(begin, end), proto(proto) {}

  ~Extern() {
    delete proto;
    proto = nullptr;
  }
};

struct File {
  std::deque<Extern*> externs;
  std::deque<Func*> funcs;

  ~File() {
    for (auto e : externs) delete e;
    for (auto e : funcs) delete e;

    externs.clear();
    funcs.clear();
  }
};

}  // namespace ast

}  // namespace felis

#endif  // FELIS_NODE_AST_H_
