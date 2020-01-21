#ifndef TOKEN_HPP
#define TOKEN_HPP
#include <string>
using namespace std;

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
  bool nl;
  bool ws;
  uint32_t offset, len;

  string str;
  uint64_t num;

  Token() : kind(Kind::END), nl(false), ws(false), offset(0), len(0){};
};

using TokenKind = Token::Kind;

class Pos {
 public:
  unsigned int line, column;

  Pos(int line = 0, int column = 0) : line(line), column(column){};

  void lines(int ln = 1) {
    line += ln;
    column = 0;
  }
  void columns(int col = 1) { column += col; }
};

class Loc {
 public:
  Pos begin, end;

  Loc(){};
  Loc(Pos &begin, Pos &end) : begin(begin), end(end){};

  void step() { begin = end; }
  void columns(int col = 1) { end.columns(col); }
  void lines(int ln = 1) { end.lines(ln); }
};

#endif
