#include "parse.hpp"

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

unique_ptr<Token>& Parser::peek() { return tokens.front(); }

unique_ptr<Token> Parser::next() {
  auto p = move(tokens.front());
  tokens.pop_front();
  return p;
}

unique_ptr<Node> Parser::parse() {
  auto expr = parseExpr();
  if (!expr) {
    // TODO: recover error
    cerr << "error!" << endl;
  }
  return expr;
}

template <typename... Args>
unique_ptr<Node> Parser::error(const char* format, Args const&... args) {
  fprintf(stderr, "%s:%d:%d: ", filename.c_str(), peek()->pos.line,
          peek()->pos.column);
  fprintf(stderr, format, args...);
  return nullptr;
};

bool is_lit(TokenKind kind) {
  return kind == TokenKind::LIT_INT || kind == TokenKind::LIT_FLOAT ||
         kind == TokenKind::LIT_STR || kind == TokenKind::LIT_CHAR ||
         kind == TokenKind::LIT_BOOL;
};

bool is_primary(TokenKind kind) {
  return kind == TokenKind::LPAREN || kind == TokenKind::IDENT || is_lit(kind);
};

unique_ptr<Node> Parser::parseExpr(uint8_t prec) {
  UnOp* unOp(nullptr);
  if (peek()->kind == TokenKind::MINUS) {
    next();
    unOp = new UnOp{UnOp::NEG};
  } else if (peek()->kind == TokenKind::NOT) {
    next();
    unOp = new UnOp{UnOp::NOT};
  }

  if (!is_primary(peek()->kind)) {
    return error("expected primary expr\n");
  }

  auto lhs = parsePrimary();
  if (!lhs) return nullptr;

  if (peek()->kind == TokenKind::LPAREN) {
    // TODO: call
    UNIMPLEMENTED("call")
  }
  if (unOp) {
    // TODO: Unary
    UNIMPLEMENTED("unary")
  };

  if (peek()->nl) {
    return lhs;
  }

  while (true) {
    if (!isBinOp(peek()->kind)) {
      return lhs;
    }
    BinOp binOp(binOpFrom(peek()->kind));
    if (binOp <= prec) return lhs;

    next();
    auto rhs = parseExpr(binOp);
    if (!rhs) return nullptr;
    lhs = make_unique<Binary>(Binary(move(lhs), binOp, move(rhs)));
  }
};

unique_ptr<Node> Parser::parsePrimary() {
  auto token = next();
  if (token->is(TokenKind::IDENT)) {
    return make_unique<Ident>(Ident(token->sval));
  } else if (token->is(TokenKind::LIT_INT)) {
    return make_unique<LitInt>(LitInt(token->ival));
  } else if (token->is(TokenKind::LIT_BOOL)) {
    return make_unique<LitBool>(LitBool(token->bval));
  } else if (token->is(TokenKind::LIT_CHAR)) {
    return make_unique<LitChar>(LitChar(token->ival));
  } else if (token->is(TokenKind::LIT_STR)) {
    return make_unique<LitStr>(LitStr(token->sval));
  } else if (token->is(TokenKind::LPAREN)) {
    auto expr = parseExpr();
    if (!expr) return nullptr;
    if (!peek()->is(TokenKind::RPAREN)) {
      return error("expected )\n");
    }
    next();
    return expr;
  } else {
    return error("unreachable primary\n");
  }
};

