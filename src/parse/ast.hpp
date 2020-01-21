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
  enum Kind { IDENT, LIT_INT, BINARY };
  virtual Kind kind() = 0;
};

class Ident : public Node {
 public:
  Node::Kind kind() { return Node::Kind::IDENT; };

  string name;

  Ident(string name) : name(name){};
};

class LitInt : public Node {
 public:
  Node::Kind kind() { return Node::Kind::LIT_INT; };

  uint64_t num;

  LitInt(uint64_t num) : num(num){};
};

class Binary : public Node {
 public:
  Node::Kind kind() { return Node::Kind::BINARY; };

  Node* lhs;
  Node* rhs;
  BinOp op;

  Binary(Node* lhs, BinOp op, Node* rhs)
      : lhs(move(lhs)), rhs(move(rhs)), op(op){};
};

#endif
