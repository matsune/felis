#ifndef PARSE_HPP
#define PARSE_HPP

#include <deque>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

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

  bool is(Kind kind) { return this->kind == kind; };
  bool isIdent() { return kind == Kind::IDENT; }
  bool isLit() { return Kind::LIT_INT <= kind && kind <= Kind::LIT_STR; };

  void debug();
};

using TokenKind = Token::Kind;

string to_string(TokenKind kind);

class Lexer {
  basic_istream<char> &in;
  string filename;
  Pos pos;

  rune peek;
  bool eat_ident(unique_ptr<Token> &);
  bool read_digits(string &s, bool f(uint32_t));
  bool eat_decimal_digits(uint64_t &);
  bool eat_num(unique_ptr<Token> &t);
  bool eat_string(string &sval);
  bool eat_char(rune &);
  bool escape(char &c);
  void eatLineComment();
  bool eatBlockComment(bool &);
  rune scan();
  rune getPeek();
  rune bump();
  template <typename... Args>
  bool error(const char *format, Args const &... args);

 public:
  Lexer(basic_istream<char> &in, string filename = "")
      : in(in), filename(filename) {
    peek = scan();
  };
  void setFilename(string filename) { this->filename = filename; };
  bool next(unique_ptr<Token> &t);
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

class Node {
 public:
  enum Kind { STMT, BLOCK };
  virtual Kind nodeKind() = 0;
};

class Stmt : public Node {
 public:
  enum Kind { EXPR, RET };
  virtual Kind stmtKind() = 0;
  Node::Kind nodeKind() { return Node::Kind::STMT; };
};

class Expr : public Stmt {
 public:
  enum Kind { IDENT, BINARY, LIT };
  virtual Kind exprKind() = 0;
  Stmt::Kind stmtKind() { return Stmt::Kind::EXPR; };
};

class Ident : public Expr {
 public:
  Expr::Kind exprKind() { return Expr::Kind::IDENT; };

  string sval;

  Ident(string sval = "") : sval(sval){};
};

class Lit : public Expr {
 public:
  enum Kind { INT, BOOL, CHAR, STR };
  virtual Kind litKind() = 0;
  Expr::Kind exprKind() { return Expr::Kind::LIT; };
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

enum UnOp { NEG, NOT };

class BinaryExpr : public Expr {
 public:
  Expr::Kind exprKind() { return Expr::Kind::BINARY; };

  unique_ptr<Node> lhs;
  unique_ptr<Node> rhs;
  BinOp op;

  BinaryExpr(unique_ptr<Node> lhs, BinOp op, unique_ptr<Node> rhs)
      : lhs(move(lhs)), rhs(move(rhs)), op(op){};
};

class RetStmt : public Stmt {
 public:
  Kind stmtKind() { return Stmt::Kind::RET; };
  unique_ptr<Expr> expr;

  RetStmt(unique_ptr<Expr> expr = unique_ptr<Expr>()) : expr(move(expr)){};
};

class Block : public Node {
 public:
  Kind nodeKind() { return Node::Kind::BLOCK; };
  vector<unique_ptr<Stmt>> stmts;
};

class Parser {
 private:
  string filename = "";
  deque<unique_ptr<Token>> tokens;
  void error(string msg);
  unique_ptr<Token> &peek();
  unique_ptr<Token> next();
  template <typename... Args>
  void error(const char *format, Args const &... args);
  unique_ptr<Expr> parseExpr(uint8_t prec = 0);
  unique_ptr<Expr> parsePrimary();
  unique_ptr<Stmt> parseStmt();
  unique_ptr<Block> parseBlock();

 public:
  void push_token(unique_ptr<Token> &&token) { tokens.push_back(move(token)); };
  unique_ptr<Node> parse();
  void setFilename(string filename) { this->filename = filename; }
};

class Printer {
  uint16_t depth = 0;
  uint32_t line = 1;
  bool afterNl = false;
  void writeLineNum();
  void indent();
  template <typename... Args>
  void write(const string format, Args const &... args);
  template <typename... Args>
  void writeln(const string format, Args const &... args);
  void down(string);
  void up(string);
  void printIdent(Ident *ident);
  void printStmt(Stmt *stmt);
  void printExpr(Expr *expr);
  void printLit(Lit *lit);

 public:
  void print(unique_ptr<Node> &node);
};

#endif
