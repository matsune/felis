#ifndef PARSE_HPP
#define PARSE_HPP

#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include "common/pos.hpp"

using namespace std;

struct rune {
  uint32_t scalar;

  bool operator==(const int32_t &c) const { return scalar == c; };
  bool operator!=(const int32_t &c) const { return scalar != c; };
  bool operator==(const rune &r) const { return scalar == r.scalar; };

  rune(uint32_t scalar = 0) : scalar(scalar){};

  int len_utf8();
  int encode_utf8(char[4]);
};

template <typename T>
static string tostring(const T &t) {
  ostringstream ss;
  ss << t;
  return ss.str();
}

class Token {
 public:
  enum Kind {
    END,  // '\0'
    IDENT,
    LIT_INT,
    LIT_FLOAT,
    LIT_BOOL,
    LIT_CHAR,
    LIT_STR,
    // keyword
    KW_RET,
    // arith_op
    PLUS,     // +
    MINUS,    // -
    STAR,     // *
    SLASH,    // /
    PERCENT,  // %
    // bitwise_op
    AND,    // &
    OR,     // |
    CARET,  // ^
    SHL,    // <<
    SHR,    // >>
    // lazy_bool_op
    ANDAND,  // &&
    OROR,    // ||
    // comp_op
    LT,    // <
    LE,    // <=
    GT,    // >
    GE,    // >=
    EQEQ,  // ==
    NEQ,   // !=
    // symbol
    NOT,     // !
    LPAREN,  // (
    RPAREN,  // )
    LBRACE,  // {
    RBRACE,  // }
    EQ,      // =
    SEMI,    // ;
    COLON,   // :
    COMMA,   // ,
    ARROW,   // ->
  };

  Kind kind = Kind::END;
  bool nl = false;
  bool ws = false;
  Pos pos;

  string sval = "";
  union {
    uint64_t ival;
    rune cval;
    double fval;
    bool bval;
  };
  void reset() {
    kind = Kind::END;
    nl = false;
    ws = false;

    sval = "";
    ival = 0;
  };

  Token() { reset(); }

  bool is(Kind kind) { return this->kind == kind; };
  bool isIdent() { return kind == Kind::IDENT; }
  bool isLit() { return Kind::LIT_INT <= kind && kind <= Kind::LIT_STR; };

  void debug();
};

using TokenKind = Token::Kind;

class Lexer {
  ifstream &in;
  string filename;
  Pos pos;

  rune peek;
  bool eat_ident(Token &);
  bool read_digits(string &s, bool f(uint32_t));
  bool eat_decimal_digits(uint64_t &);
  bool eat_num(Token &t);
  bool eat_string(string &sval);
  bool eat_char(rune &);
  bool escape(char &c);
  void eatLineComment();
  bool eatBlockComment(bool &);
  rune scan();
  template <typename... Args>
  bool error(const char *format, Args const &... args);

 public:
  Lexer(ifstream &in, string filename = "") : in(in), filename(filename) {
    peek = scan();
  };
  void setFilename(string filename) { this->filename = filename; };
  rune getPeek();
  rune bump();
  bool next(Token &t);
};

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

class Printer {
  uint16_t depth = 0;
  uint32_t line = 1;
  bool afterNl = false;
  void writeLineNum();
  void indent();
  void write(string msg);
  void writeln(string msg);
  void down(string);
  void up(string);

 public:
  void print(unique_ptr<Node> &node);
};

#endif
