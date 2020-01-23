#include "parser.hpp"
#include "token.hpp"

#define UNIMPLEMENTED(msg)                   \
  (cerr << "unimplemented " << msg << endl); \
  exit(1);

bool isBinOp(Token::Kind kind) {
  switch (kind) {
    case TokenKind::PLUS:
    case TokenKind::MINUS:
    case TokenKind::STAR:
    case TokenKind::SLASH:
    case TokenKind::PERCENT:
    case TokenKind::LT:
    case TokenKind::LE:
    case TokenKind::GT:
    case TokenKind::GE:
      return true;
    default:
      return false;
  }
};
BinOp binOpFrom(TokenKind kind) {
  switch (kind) {
    case TokenKind::PLUS:
      return BinOp::ADD;
    case TokenKind::MINUS:
      return BinOp::SUB;
    case TokenKind::STAR:
      return BinOp::MUL;
    case TokenKind::SLASH:
      return BinOp::DIV;
    case TokenKind::PERCENT:
      return BinOp::MOD;
    case TokenKind::GT:
      return BinOp::GT;
    case TokenKind::GE:
      return BinOp::GE;
    case TokenKind::LT:
      return BinOp::LT;
    case TokenKind::LE:
      return BinOp::LE;
    default:
      throw "unreachable";
  }
}

Parser::Parser(unique_ptr<Lexer> lexer, FLEX_STD istream& yyin,
               FLEX_STD ostream& yyout) {
  lexer->switch_streams(yyin, yyout);
  lexer->yylex(peek);
  lexer->yylex(peek2);
  this->lexer = move(lexer);
};

Token Parser::next() {
  auto p = move(peek);
  peek = move(peek2);
  lexer->yylex(peek2);
  return p;
}

unique_ptr<Node> Parser::parse() {
  auto expr = parseExpr();
  if (!expr) {
    cerr << "Error occcurred!" << endl;
    // TODO: recover error
  }
  return expr;
}

unique_ptr<Node> Parser::parseExpr(uint8_t prec) {
  UnOp* unOp(NULL);
  if (peek.is(TokenKind::MINUS)) {
    next();
    unOp = new UnOp{UnOp::NEG};
  } else if (peek.is(TokenKind::NOT)) {
    next();
    unOp = new UnOp{UnOp::NOT};
  }

  if (!peek.is(TokenKind::LPAREN) && !peek.isIdent() && !peek.isLit()) {
    // TODO: push unexpected token error
    return nullptr;
  }

  auto lhs = parsePrimary();
  if (peek.is(TokenKind::LPAREN)) {
    // TODO: call
    UNIMPLEMENTED("call")
  }
  if (unOp) {
    // TODO: Unary
    UNIMPLEMENTED("unary")
  };

  if (peek.nl) {
    return lhs;
  }

  while (true) {
    if (!isBinOp(peek.kind)) {
      return lhs;
    }
    BinOp binOp(binOpFrom(peek.kind));
    if (binOp <= prec) return lhs;

    next();
    auto rhs = parseExpr(binOp);
    lhs = make_unique<Binary>(Binary(move(lhs), binOp, move(rhs)));
  }
};

unique_ptr<Node> Parser::parsePrimary() {
  auto token = next();
  if (token.is(TokenKind::IDENT)) {
    return make_unique<Ident>(Ident(token.sval));
  } else if (token.is(TokenKind::LIT_INT)) {
    return make_unique<LitInt>(LitInt(token.ival));
  } else if (token.is(TokenKind::LIT_BOOL)) {
    return make_unique<LitBool>(LitBool(token.bval));
  } else {
    UNIMPLEMENTED("primary")
  }
};

