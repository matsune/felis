#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <sstream>
#include <string>

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
    UNKNOWN,
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

  Kind kind = Kind::END;
  bool nl = false;
  bool ws = false;
  uint32_t offset = 0, len = 0;

  string sval = "";
  uint64_t ival = 0;
  bool bval = false;

  bool is(Kind kind) { return this->kind == kind; };
  bool isIdent() { return kind == Kind::IDENT; }
  bool isLit() { return Kind::LIT_INT <= kind && kind <= Kind::LIT_STR; };
  string to_string() {
    switch (kind) {
      case END:
        return "END";
      case IDENT:
        return sval;
      case LIT_INT:
        return tostring(ival);
      case LIT_FLOAT:
        return "";
      case LIT_BOOL:
        return bval ? "true" : "false";
      case LIT_CHAR:
        return "CHAR";
      case LIT_STR:
        return sval;
      case PLUS:
        return "+";
      case MINUS:
        return "-";
      case STAR:
        return "*";
      case SLASH:
        return "/";
      case PERCENT:
        return "%";
      case AND:
        return "&";
      case OR:
        return "|";
      case CARET:
        return "^";
      case SHL:
        return "<<";
      case SHR:
        return ">>";
      case ANDAND:
        return "&&";
      case OROR:
        return "||";
      case LT:
        return "<";
      case LE:
        return "<=";
      case GT:
        return ">";
      case GE:
        return ">=";
      case EQEQ:
        return "==";
      case NEQ:
        return "!=";
      case NOT:
        return "!";
      case LPAREN:
        return "(";
      case RPAREN:
        return ")";
      case LBRACE:
        return "{";
      case RBRACE:
        return "}";
      case EQ:
        return "=";
      case SEMI:
        return ";";
      case COLON:
        return ":";
      case COMMA:
        return ",";
      case ARROW:
        return "->";
      default:
        return "unknown";
    }
  };
};

using TokenKind = Token::Kind;

#endif
