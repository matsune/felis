#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <string>
#include "rune.hpp"

using namespace std;

class Pos {
 public:
  unsigned int line, column;

  Pos(int line = 1, int column = 1) : line(line), column(column){};

  void lines(int ln = 1) {
    line += ln;
    column = 1;
  }
  void columns(int col = 1) { column += col; }
};

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
    KW_FN,
    KW_LET,
    KW_VAR,
    KW_RET,
    KW_EXT,
    KW_IF,
    KW_ELSE,
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

  bool nl;
  bool ws;
  Pos pos;

  string sval;
  uint64_t ival;
  rune cval;
  double fval;
  bool bval;

  void reset() {
    kind = Kind::END;
    nl = false;
    ws = false;

    sval = "";
    ival = 0;
    cval = rune(0);
    fval = 0;
    bval = false;
  };

  bool isIdent() { return kind == Kind::IDENT; }
  bool isLit() { return Kind::LIT_INT <= kind && kind <= Kind::LIT_STR; };

  void debug();
};

using TokenKind = Token::Kind;

string to_string(TokenKind kind);

#endif
