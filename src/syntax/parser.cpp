#include "parser.hpp"
#include <cassert>

#define EXPECT(KIND)                                 \
  if (peek()->kind != KIND) {                        \
    error("expected %s\n", to_string(KIND).c_str()); \
    return nullptr;                                  \
  }

bool is_lit(TokenKind kind) {
  return kind == TokenKind::LIT_INT || kind == TokenKind::LIT_FLOAT ||
         kind == TokenKind::LIT_STR || kind == TokenKind::LIT_CHAR ||
         kind == TokenKind::LIT_BOOL;
};

bool is_primary(TokenKind kind) {
  return kind == TokenKind::LPAREN || kind == TokenKind::IDENT || is_lit(kind);
};

bool is_bin_op(TokenKind kind) {
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
      UNREACHABLE(binOp)
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

unique_ptr<Extern> Parser::parseExtern() {
  assert(bump()->kind == TokenKind::KW_EXT);
  auto proto = parseFnProto();
  if (!proto) return nullptr;
  return make_unique<Extern>(move(proto));
};

unique_ptr<FnDecl> Parser::parseFnDecl() {
  auto proto = parseFnProto();
  if (!proto) return nullptr;
  auto block = parseBlock();
  if (!block) return nullptr;
  return make_unique<FnDecl>(move(proto), move(block));
};

unique_ptr<FnProto> Parser::parseFnProto() {
  EXPECT(TokenKind::KW_FN);
  bump();

  EXPECT(TokenKind::IDENT);
  auto name = make_unique<Ident>(bump()->sval);

  vector<unique_ptr<FnArg>> args;
  if (!parseFnArgs(args)) return nullptr;

  if (peek()->kind == TokenKind::ARROW) {
    bump();
    if (peek()->kind != TokenKind::IDENT) {
      error("expected ident\n");
      return nullptr;
    }
    auto ty = make_unique<Ident>(bump()->sval);
    return make_unique<FnProto>(move(name), move(args), move(ty));
  } else {
    return make_unique<FnProto>(move(name), move(args));
  }
};

bool Parser::parseFnArgs(vector<unique_ptr<FnArg>>& args) {
  if (peek()->kind != TokenKind::LPAREN) {
    error("expected (\n");
    return false;
  }
  bump();

  if (peek()->kind == TokenKind::RPAREN) {
    bump();
    return true;
  }

  auto arg = parseFnArg();
  if (!arg) return false;
  bool hasName = arg->withName();
  args.push_back(move(arg));

  while (peek()->kind == TokenKind::COMMA) {
    bump();

    auto arg = parseFnArg();
    if (!arg) return false;
    if (hasName != arg->withName()) {
      error("mixed name in func args\n");
      return false;
    }
    args.push_back(move(arg));
  }

  if (peek()->kind != TokenKind::RPAREN) {
    error("expected )\n");
    return false;
  }
  bump();

  return true;
}

unique_ptr<FnArg> Parser::parseFnArg() {
  EXPECT(TokenKind::IDENT);
  auto nameOrTy = make_unique<Ident>(bump()->sval);

  if (peek()->kind == TokenKind::COLON) {
    bump();

    EXPECT(TokenKind::IDENT);

    auto ty = make_unique<Ident>(bump()->sval);
    return make_unique<FnArg>(move(ty), move(nameOrTy));
  } else {
    return make_unique<FnArg>(move(nameOrTy));
  }
};

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
    if (lhs->exprKind() != Expr::Kind::IDENT) {
      error("cannot call non function\n");
      return nullptr;
    }
    EXPECT(TokenKind::LPAREN);
    bump();

    Ident* ident_ptr = (Ident*)lhs.get();
    lhs.release();
    auto ident = unique_ptr<Ident>(ident_ptr);

    vector<unique_ptr<Expr>> args;
    if (peek()->kind == TokenKind::RPAREN) {
      bump();
    } else {
      auto first = parseExpr();
      if (!first) return nullptr;
      args.push_back(move(first));

      if (peek()->kind == TokenKind::RPAREN) {
        return make_unique<CallExpr>(move(ident), move(args));
      }

      while (peek()->kind != TokenKind::RPAREN) {
        EXPECT(TokenKind::COMMA);
        bump();

        auto arg = parseExpr();
        if (!arg) return nullptr;
        args.push_back(move(arg));
      }
      bump();
    }
    lhs = make_unique<CallExpr>(move(ident), move(args));
  }
  if (unOp) {
    return make_unique<UnaryExpr>(move(unOp), move(lhs));
  };

  if (peek()->nl) {
    return lhs;
  }

  while (true) {
    if (!is_bin_op(peek()->kind)) {
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
    EXPECT(TokenKind::RPAREN);
    bump();
    return expr;
  }
  return nullptr;
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
    return make_unique<RetStmt>(move(expr));
  } else if (peek()->kind == TokenKind::KW_LET ||
             peek()->kind == TokenKind::KW_VAR) {
    bool isLet = bump()->kind == TokenKind::KW_LET;

    EXPECT(TokenKind::IDENT);
    auto name = make_unique<Ident>(bump()->sval);

    EXPECT(TokenKind::EQ);
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
  return parseExpr();
}

unique_ptr<IfStmt> Parser::parseIfStmt() {
  EXPECT(TokenKind::KW_IF);
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

unique_ptr<Block> Parser::parseBlock() {
  EXPECT(TokenKind::LBRACE);
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

template <typename... Args>
void Parser::error(const char* format, Args const&... args) {
  fprintf(stderr, "%s:%d:%d: ", filename.c_str(), peek()->pos.line,
          peek()->pos.column);
  fprintf(stderr, format, args...);
};

unique_ptr<File> Parser::parse() {
  auto file = make_unique<File>();

  bool needsNl = false;
  while (peek()->kind != TokenKind::END) {
    if (needsNl && (peek()->kind != TokenKind::SEMI && !peek()->nl)) {
      error("needs \\n or ;");
      return nullptr;
    }
    if (peek()->kind == TokenKind::KW_EXT) {
      auto ext = parseExtern();
      if (!ext) return nullptr;
      file->externs.push_back(move(ext));
    } else if (peek()->kind == TokenKind::KW_FN) {
      auto fn = parseFnDecl();
      if (!fn) return nullptr;
      file->fnDecls.push_back(move(fn));
    } else {
      error("unknown top-level token\n");
      return nullptr;
    }
    needsNl = true;
  }
  return file;
}

