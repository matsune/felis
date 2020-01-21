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

Token Parser::next() {
  auto p = move(peek);
  peek = move(peek2);
  lexer.yylex(peek2);
  return p;
}

void Parser::parse() {}

unique_ptr<Node> Parser::parseExpr(uint8_t prec) {
  UnOp* unOp(NULL);
  if (isNext(TokenKind::MINUS)) {
    next();
    unOp = new UnOp{UnOp::NEG};
  } else if (isNext(TokenKind::NOT)) {
    next();
    unOp = new UnOp{UnOp::NOT};
  }

  // TODO: check LPAREN or IDENT or LIT

  auto lhs = parsePrimary();
  if (isNext(TokenKind::LPAREN)) {
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
    lhs = make_unique<Node>(Binary(move(lhs), binOp, move(rhs)));
  }
};

unique_ptr<Node> Parser::parsePrimary() {
  auto token = next();
  if (token.kind == TokenKind::IDENT) {
    return make_unique<Node>(Ident(token.str));
  } else if (token.kind == TokenKind::LIT_INT) {
    return make_unique<Node>(LitInt(token.num));
  } else {
    UNIMPLEMENTED("primary")
  }
};

bool Parser::isNext(TokenKind kind) { return peek.kind == kind; };

