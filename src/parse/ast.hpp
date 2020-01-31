#ifndef AST_HPP
#define AST_HPP
#include <iostream>
#include <memory>
#include "token.hpp"

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

enum UnOp { NEG, NOT };

class Node {
 public:
  enum Kind { IDENT, LIT, BINARY };
  virtual Kind nodeKind() = 0;
};

class Ident : public Node {
 public:
  Node::Kind nodeKind() { return Node::Kind::IDENT; };

  string sval;

  Ident(string sval = "") : sval(sval){};
};

class Lit : public Node {
 public:
  enum Kind { INT, BOOL, CHAR, STR };
  virtual Kind litKind() = 0;
  Node::Kind nodeKind() { return Node::Kind::LIT; };
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

class Binary : public Node {
 public:
  Node::Kind nodeKind() { return Node::Kind::BINARY; };

  unique_ptr<Node> lhs;
  unique_ptr<Node> rhs;
  BinOp op;

  Binary(unique_ptr<Node> lhs, BinOp op, unique_ptr<Node> rhs)
      : lhs(move(lhs)), rhs(move(rhs)), op(op){};
};

#endif
