#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <sstream>
#include <string>
#include "common/pos.hpp"

using namespace std;

template <typename T>
static string tostring(const T& t) {
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
  uint64_t ival = 0;
  double fval = 0;
  bool bval = false;

  void reset() {
    kind = Kind::END;
    nl = false;
    ws = false;

    sval = "";
    ival = 0;
    fval = 0;
    bval = false;
  };

  bool is(Kind kind) { return this->kind == kind; };
  bool isIdent() { return kind == Kind::IDENT; }
  bool isLit() { return Kind::LIT_INT <= kind && kind <= Kind::LIT_STR; };

  void debug();
};

using TokenKind = Token::Kind;

#endif
