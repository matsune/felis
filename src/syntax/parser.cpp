#include "parser.hpp"

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

unique_ptr<Token>& Parser::peek2() {
  if (tokens.size() < 2) return tokens.front();
  return tokens[1];
}

unique_ptr<Token> Parser::bump() {
  auto p = move(tokens.front());
  tokens.pop_front();
  return p;
}

unique_ptr<File> Parser::parse() {
  auto file = make_unique<File>();

  /* auto block = parseBlock(); */
  /* if (!block) { */
  /*   // TODO: recover error */
  /*   cerr << "error!" << endl; */
  /* } */
  return file;
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
    bump();
    if (peek()->kind == TokenKind::RBRACE || peek()->kind == TokenKind::SEMI) {
      bump();
      return make_unique<RetStmt>();
    }
    auto expr = parseExpr();
    if (!expr) return nullptr;
    auto retStmt = make_unique<RetStmt>(move(expr));
    return retStmt;
  } else if (peek()->kind == TokenKind::KW_LET ||
             peek()->kind == TokenKind::KW_VAR) {
    bool isLet = bump()->kind == TokenKind::KW_LET;
    if (peek()->kind != TokenKind::IDENT) {
      error("expected ident\n");
      return nullptr;
    }
    auto name = make_unique<Ident>(bump()->sval);
    if (peek()->kind != TokenKind::EQ) {
      error("expected =\n");
      return nullptr;
    }
    bump();

    auto expr = parseExpr();
    if (!expr) return nullptr;

    return make_unique<VarDeclStmt>(isLet, move(name), move(expr));
  } else if (peek()->kind == TokenKind::KW_IF) {
    return parseIfStmt();
  } else if (peek()->kind == TokenKind::IDENT) {
    if (peek2()->kind == TokenKind::EQ) {
      // assign stmt
      auto name = make_unique<Ident>(bump()->sval);
      bump();
      auto expr = parseExpr();
      if (!expr) return nullptr;
      return make_unique<AssignStmt>(move(name), move(expr));
    }
  }
  auto expr = parseExpr();
  return expr;
}

unique_ptr<IfStmt> Parser::parseIfStmt() {
  if (peek()->kind != TokenKind::KW_IF) {
    error("expected if\n");
    return nullptr;
  }
  bump();

  auto cond = parseExpr();
  if (!cond) return nullptr;
  auto block = parseBlock();
  if (!block) return nullptr;

  if (peek()->kind != TokenKind::KW_ELSE) {
    return make_unique<IfStmt>(move(cond), move(block));
  }
  bump();

  if (peek()->kind == TokenKind::KW_IF) {
    auto els = parseIfStmt();
    if (!els) return nullptr;
    return make_unique<IfStmt>(move(cond), move(block), move(els));
  } else {
    auto els = parseBlock();
    if (!els) return nullptr;
    return make_unique<IfStmt>(move(cond), move(block), move(els));
  }
}

unique_ptr<Expr> Parser::parseExpr(uint8_t prec) {
  unique_ptr<UnOp> unOp;
  if (peek()->kind == TokenKind::MINUS) {
    bump();
    unOp = make_unique<UnOp>(UnOp::NEG);
  } else if (peek()->kind == TokenKind::NOT) {
    bump();
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

    bump();
    auto rhs = parseExpr(binOp);
    if (!rhs) return nullptr;
    lhs = make_unique<BinaryExpr>(move(lhs), binOp, move(rhs));
  }
};

unique_ptr<Expr> Parser::parsePrimary() {
  auto token = bump();
  if (token->kind == TokenKind::IDENT) {
    return make_unique<Ident>(token->sval);
  } else if (token->kind == TokenKind::LIT_INT) {
    return make_unique<LitInt>(token->ival);
  } else if (token->kind == TokenKind::LIT_BOOL) {
    return make_unique<LitBool>(token->bval);
  } else if (token->kind == TokenKind::LIT_CHAR) {
    return make_unique<LitChar>(token->ival);
  } else if (token->kind == TokenKind::LIT_STR) {
    return make_unique<LitStr>(token->sval);
  } else if (token->kind == TokenKind::LPAREN) {
    auto expr = parseExpr();
    if (!expr) return nullptr;
    if (peek()->kind != TokenKind::RPAREN) {
      error("expected )\n");
      return nullptr;
    }
    bump();
    return expr;
  } else {
    UNREACHABLE
  }
};

unique_ptr<Block> Parser::parseBlock() {
  if (peek()->kind != TokenKind::LBRACE) return nullptr;
  bump();

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
      bump();
    }
  }
  bump();
  return block;
}
