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
  enum Kind { IDENT, LIT_INT, LIT_BOOL, BINARY };
  virtual Kind kind() = 0;
};

class Ident : public Node {
 public:
  Node::Kind kind() { return Node::Kind::IDENT; };

  string sval;

  Ident(string sval = "") : sval(sval){};
};

class LitInt : public Node {
 public:
  Node::Kind kind() { return Node::Kind::LIT_INT; };

  uint64_t ival;

  LitInt(uint64_t ival = 0) : ival(ival){};
};

class LitBool : public Node {
 public:
  Node::Kind kind() { return Node::Kind::LIT_BOOL; };

  bool bval;

  LitBool(bool bval = false) : bval(bval){};
};

class Binary : public Node {
 public:
  Node::Kind kind() { return Node::Kind::BINARY; };

  unique_ptr<Node> lhs;
  unique_ptr<Node> rhs;
  BinOp op;

  Binary(unique_ptr<Node> lhs, BinOp op, unique_ptr<Node> rhs)
      : lhs(move(lhs)), rhs(move(rhs)), op(op){};
};

#endif
