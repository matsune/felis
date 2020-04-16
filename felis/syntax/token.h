#ifndef FELIS_SYNTAX_TOKEN_H_
#define FELIS_SYNTAX_TOKEN_H_

#include <string>

#include "syntax/pos.h"
#include "syntax/rune.h"

namespace felis {

struct Tok {
  union {
    std::string sval;
    uint64_t ival;
    rune cval;
    double fval;
    bool bval;
  };
};

struct Token {
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

  Kind kind;
  Pos pos;
  bool ws;
  bool nl;

  std::string sval;
  uint64_t ival;
  rune cval;
  double fval;
  bool bval;

  Token(Kind kind = Kind::END)
      : kind(kind),
        pos(Pos()),
        ws(false),
        nl(false),
        sval(""),
        ival(0),
        cval(rune(0)),
        fval(0),
        bval(false) {}

  void debug();
};

using TokenKind = Token::Kind;

}  // namespace felis

#endif  // FELIS_SYNTAX_TOKEN_H_
