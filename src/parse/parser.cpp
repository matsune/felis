#include "parse.hpp"

#define UNIMPLEMENTED throw FatalError("unimplemented");
#define UNREACHABLE throw FatalError("unreachable");

class FatalError {
  string msg;

 public:
  FatalError(string msg) : msg(msg){};
};

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
      UNREACHABLE
  }
}

unique_ptr<Token>& Parser::peek() { return tokens.front(); }

unique_ptr<Token> Parser::next() {
  auto p = move(tokens.front());
  tokens.pop_front();
  return p;
}

unique_ptr<Node> Parser::parse() {
  auto block = parseBlock();
  if (!block) {
    // TODO: recover error
    cerr << "error!" << endl;
  }
  return block;
}

template <typename... Args>
void Parser::error(const char* format, Args const&... args) {
  fprintf(stderr, "%s:%d:%d: ", filename.c_str(), peek()->pos.line,
          peek()->pos.column);
  fprintf(stderr, format, args...);
};

bool is_lit(TokenKind kind) {
  return kind == TokenKind::LIT_INT || kind == TokenKind::LIT_FLOAT ||
         kind == TokenKind::LIT_STR || kind == TokenKind::LIT_CHAR ||
         kind == TokenKind::LIT_BOOL;
};

bool is_primary(TokenKind kind) {
  return kind == TokenKind::LPAREN || kind == TokenKind::IDENT || is_lit(kind);
};

unique_ptr<Stmt> Parser::parseStmt() {
  if (peek()->kind == TokenKind::KW_RET) {
    next();
    if (peek()->kind == TokenKind::RBRACE || peek()->kind == TokenKind::SEMI) {
      next();
      return make_unique<RetStmt>();
    }
    auto expr = parseExpr();
    if (!expr) return nullptr;
    auto retStmt = make_unique<RetStmt>(move(expr));
    return retStmt;
  } else {
    auto expr = parseExpr();
    return expr;
  }
}

unique_ptr<Expr> Parser::parseExpr(uint8_t prec) {
  unique_ptr<UnOp> unOp;
  if (peek()->kind == TokenKind::MINUS) {
    next();
    unOp = make_unique<UnOp>(UnOp::NEG);
  } else if (peek()->kind == TokenKind::NOT) {
    next();
    unOp = make_unique<UnOp>(UnOp::NOT);
  }

  if (!is_primary(peek()->kind)) {
    error("expected primary expr\n");
    return nullptr;
  }

  auto lhs = parsePrimary();
  if (!lhs) return nullptr;

  if (peek()->kind == TokenKind::LPAREN) {
    // TODO: call
    UNIMPLEMENTED
  }
  if (unOp) {
    // TODO: Unary
    UNIMPLEMENTED
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
    lhs = make_unique<BinaryExpr>(move(lhs), binOp, move(rhs));
  }
};

unique_ptr<Expr> Parser::parsePrimary() {
  auto token = next();
  if (token->is(TokenKind::IDENT)) {
    return make_unique<Ident>(token->sval);
  } else if (token->is(TokenKind::LIT_INT)) {
    return make_unique<LitInt>(token->ival);
  } else if (token->is(TokenKind::LIT_BOOL)) {
    return make_unique<LitBool>(token->bval);
  } else if (token->is(TokenKind::LIT_CHAR)) {
    return make_unique<LitChar>(token->ival);
  } else if (token->is(TokenKind::LIT_STR)) {
    return make_unique<LitStr>(token->sval);
  } else if (token->is(TokenKind::LPAREN)) {
    auto expr = parseExpr();
    if (!expr) return nullptr;
    if (!peek()->is(TokenKind::RPAREN)) {
      error("expected )\n");
      return nullptr;
    }
    next();
    return expr;
  } else {
    UNREACHABLE
  }
};

unique_ptr<Block> Parser::parseBlock() {
  if (peek()->kind != TokenKind::LBRACE) return nullptr;
  next();

  auto block = make_unique<Block>();
  while (true) {
    if (peek()->kind == TokenKind::RBRACE) {
      break;
    }
    auto stmt = parseStmt();
    if (!stmt) return nullptr;
    block->stmts.push_back(move(stmt));
    if (peek()->kind == TokenKind::RBRACE) {
      break;
    }
    if (peek()->kind == TokenKind::SEMI) {
      next();
    }
  }
  return block;
}
