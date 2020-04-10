#include "syntax/parser.h"

#include <cassert>
#include <vector>

#include "string/string.h"

namespace felis {

namespace {  // fileprivate

inline bool is_lit(TokenKind kind) {
  return kind == TokenKind::LIT_INT || kind == TokenKind::LIT_FLOAT ||
         kind == TokenKind::LIT_STR || kind == TokenKind::LIT_CHAR ||
         kind == TokenKind::LIT_BOOL;
}

inline bool is_primary(TokenKind kind) {
  return kind == TokenKind::LPAREN || kind == TokenKind::IDENT || is_lit(kind);
}

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
}

inline BinOp binOpFrom(TokenKind kind) {
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
      assert(false);
  }
}

}  // namespace

std::unique_ptr<Token>& Parser::Peek() { return tokens_.front(); }

std::unique_ptr<Token>& Parser::Peek2() {
  if (tokens_.size() < 2) return tokens_.front();
  return tokens_[1];
}

std::unique_ptr<Token> Parser::Bump() {
  auto p = std::move(tokens_.front());
  tokens_.pop_front();
  return p;
}

std::unique_ptr<Extern> Parser::ParseExtern() {
  auto ext = Bump();
  assert(ext->kind == TokenKind::KW_EXT);
  auto pos = ext->pos;
  auto proto = ParseFnProto();
  if (!proto) return nullptr;
  return std::make_unique<Extern>(nextId_++, pos, std::move(proto));
}

std::unique_ptr<FnDecl> Parser::ParseFnDecl() {
  auto proto = ParseFnProto();
  if (!proto) return nullptr;
  auto block = ParseBlock();
  if (!block) return nullptr;
  return std::make_unique<FnDecl>(nextId_++, std::move(proto),
                                  std::move(block));
}

std::unique_ptr<FnProto> Parser::ParseFnProto() {
  if (Peek()->kind != TokenKind::KW_FN) {
    Raise("expected %s", ToString(TokenKind::KW_FN).c_str());
    return nullptr;
  }
  auto fn = Bump();
  Pos fnPos = fn->pos;

  if (Peek()->kind != TokenKind::IDENT) {
    Raise("expected %s", ToString(TokenKind::IDENT).c_str());
    return nullptr;
  }
  auto nameTok = Bump();
  auto name = std::make_unique<Ident>(nameTok->pos, nameTok->sval);

  std::vector<std::unique_ptr<FnArg>> args;
  if (!ParseFnArgs(args)) return nullptr;

  if (Peek()->kind == TokenKind::ARROW) {
    Bump();
    if (Peek()->kind != TokenKind::IDENT) {
      Raise("expected ident");
      return nullptr;
    }
    auto tyTok = Bump();
    auto ty = std::make_unique<Ident>(tyTok->pos, tyTok->sval);
    return std::make_unique<FnProto>(fnPos, std::move(name), std::move(args),
                                     std::move(ty));
  } else {
    return std::make_unique<FnProto>(fnPos, std::move(name), std::move(args));
  }
}

// start with '('
bool Parser::ParseFnArgs(std::vector<std::unique_ptr<FnArg>>& args) {
  if (Peek()->kind != TokenKind::LPAREN) {
    Raise("expected (");
    return false;
  }
  Bump();

  if (Peek()->kind == TokenKind::RPAREN) {
    Bump();
    return true;
  }

  auto arg = ParseFnArg();
  if (!arg) return false;
  bool hasName = arg->withName();
  args.push_back(std::move(arg));

  while (Peek()->kind == TokenKind::COMMA) {
    Bump();

    auto arg = ParseFnArg();
    if (!arg) return false;
    if (hasName != arg->withName()) {
      Raise("mixed name in func args");
      return false;
    }
    args.push_back(std::move(arg));
  }

  if (Peek()->kind != TokenKind::RPAREN) {
    Raise("expected )");
    return false;
  }
  Bump();

  return true;
}

std::unique_ptr<FnArg> Parser::ParseFnArg() {
  if (Peek()->kind != TokenKind::IDENT) {
    Raise("expected %s", ToString(TokenKind::IDENT).c_str());
    return nullptr;
  }
  auto nameOrTyTok = Bump();
  auto nameOrTy = std::make_unique<Ident>(nameOrTyTok->pos, nameOrTyTok->sval);

  if (Peek()->kind == TokenKind::COLON) {
    Bump();

    if (Peek()->kind != TokenKind::IDENT) {
      Raise("expected %s", ToString(TokenKind::IDENT).c_str());
      return nullptr;
    }

    auto tyTok = Bump();
    auto ty = std::make_unique<Ident>(tyTok->pos, tyTok->sval);
    return std::make_unique<FnArg>(std::move(ty), std::move(nameOrTy));
  } else {
    return std::make_unique<FnArg>(std::move(nameOrTy));
  }
}

std::unique_ptr<Expr> Parser::ParseExpr(uint8_t prec) {
  std::unique_ptr<UnOp> unOp;
  Pos pos;
  if (Peek()->kind == TokenKind::MINUS) {
    pos = Bump()->pos;
    unOp = std::make_unique<UnOp>(UnOp::NEG);
  } else if (Peek()->kind == TokenKind::NOT) {
    pos = Bump()->pos;
    unOp = std::make_unique<UnOp>(UnOp::NOT);
  }

  if (!is_primary(Peek()->kind)) {
    Raise("expected primary expr");
    return nullptr;
  }

  auto lhs = ParsePrimary();
  if (!lhs) return nullptr;

  if (Peek()->kind == TokenKind::LPAREN) {
    if (lhs->ExprKind() != Expr::Kind::IDENT) {
      Raise("cannot call non function");
      return nullptr;
    }
    if (Peek()->kind != TokenKind::LPAREN) {
      Raise("expected %s", ToString(TokenKind::LPAREN).c_str());
      return nullptr;
    }
    Bump();

    auto ident_ptr = reinterpret_cast<Ident*>(lhs.get());
    lhs.release();
    auto ident = std::unique_ptr<Ident>(ident_ptr);

    std::vector<std::unique_ptr<Expr>> args;
    if (Peek()->kind == TokenKind::RPAREN) {
      Bump();
    } else {
      auto first = ParseExpr();
      if (!first) return nullptr;
      args.push_back(std::move(first));

      if (Peek()->kind == TokenKind::RPAREN) {
        return std::make_unique<CallExpr>(std::move(ident), std::move(args));
      }

      while (Peek()->kind != TokenKind::RPAREN) {
        if (Peek()->kind != TokenKind::COMMA) {
          Raise("expected %s", ToString(TokenKind::COMMA).c_str());
          return nullptr;
        }
        Bump();

        auto arg = ParseExpr();
        if (!arg) return nullptr;
        args.push_back(std::move(arg));
      }
      Bump();
    }
    lhs = std::make_unique<CallExpr>(std::move(ident), std::move(args));
  }
  if (unOp) {
    return std::make_unique<UnaryExpr>(pos, std::move(unOp), std::move(lhs));
  }

  if (Peek()->nl) {
    return lhs;
  }

  while (true) {
    if (!is_bin_op(Peek()->kind)) {
      return lhs;
    }
    BinOp binOp(binOpFrom(Peek()->kind));
    if (binOp <= prec) return lhs;

    Bump();
    auto rhs = ParseExpr(binOp);
    if (!rhs) return nullptr;
    lhs = std::make_unique<BinaryExpr>(std::move(lhs), binOp, std::move(rhs));
  }
}

std::unique_ptr<Expr> Parser::ParsePrimary() {
  auto token = Bump();
  Pos pos = token->pos;
  if (token->kind == TokenKind::IDENT) {
    return std::make_unique<Ident>(pos, token->sval);
  } else if (token->kind == TokenKind::LIT_INT) {
    return std::make_unique<LitInt>(pos, token->ival);
  } else if (token->kind == TokenKind::LIT_FLOAT) {
    return std::make_unique<LitFloat>(pos, token->fval);
  } else if (token->kind == TokenKind::LIT_BOOL) {
    return std::make_unique<LitBool>(pos, token->bval);
  } else if (token->kind == TokenKind::LIT_CHAR) {
    return std::make_unique<LitChar>(pos, token->cval);
  } else if (token->kind == TokenKind::LIT_STR) {
    return std::make_unique<LitStr>(pos, token->sval);
  } else if (token->kind == TokenKind::LPAREN) {
    auto expr = ParseExpr();
    if (!expr) return nullptr;
    if (Peek()->kind != TokenKind::RPAREN) {
      Raise("expected %s", ToString(TokenKind::RPAREN).c_str());
      return nullptr;
    }
    Bump();
    return expr;
  }
  return nullptr;
}

std::unique_ptr<Stmt> Parser::ParseStmt() {
  if (Peek()->kind == TokenKind::KW_RET) {
    Pos pos = Bump()->pos;
    if (Peek()->kind == TokenKind::SEMI) {
      Bump();
      return std::make_unique<RetStmt>(pos);
    }
    if (Peek()->nl) {
      return std::make_unique<RetStmt>(pos);
    }
    auto expr = ParseExpr();
    if (!expr) return nullptr;
    return std::make_unique<RetStmt>(pos, std::move(expr));
  } else if (Peek()->kind == TokenKind::KW_LET ||
             Peek()->kind == TokenKind::KW_VAR) {
    auto kw = Bump();
    bool isLet = kw->kind == TokenKind::KW_LET;
    Pos pos = kw->pos;

    if (Peek()->kind != TokenKind::IDENT) {
      Raise("expected %s", ToString(TokenKind::IDENT).c_str());
      return nullptr;
    }
    auto name = std::make_unique<Ident>(pos, Bump()->sval);

    if (Peek()->kind != TokenKind::EQ) {
      Raise("expected %s", ToString(TokenKind::EQ).c_str());
      return nullptr;
    }
    Bump();

    auto expr = ParseExpr();
    if (!expr) return nullptr;

    return std::make_unique<VarDeclStmt>(pos, isLet, std::move(name),
                                         std::move(expr));
  } else if (Peek()->kind == TokenKind::KW_IF) {
    return ParseIfStmt();
  } else if (Peek()->kind == TokenKind::IDENT) {
    if (Peek2()->kind == TokenKind::EQ) {
      // assign stmt
      auto bump = Bump();
      auto name = std::make_unique<Ident>(bump->pos, bump->sval);
      Bump();
      auto expr = ParseExpr();
      if (!expr) return nullptr;
      return std::make_unique<AssignStmt>(std::move(name), std::move(expr));
    }
  }
  return ParseExpr();
}

std::unique_ptr<IfStmt> Parser::ParseIfStmt() {
  if (Peek()->kind != TokenKind::KW_IF) {
    Raise("expected %s", ToString(TokenKind::KW_IF).c_str());
    return nullptr;
  }
  Pos pos = Bump()->pos;

  auto cond = ParseExpr();
  if (!cond) return nullptr;
  auto block = ParseBlock();
  if (!block) return nullptr;

  if (Peek()->kind != TokenKind::KW_ELSE) {
    return std::make_unique<IfStmt>(pos, std::move(cond), std::move(block));
  }
  Bump();

  if (Peek()->kind == TokenKind::KW_IF) {
    auto els = ParseIfStmt();
    if (!els) return nullptr;
    return std::make_unique<IfStmt>(pos, std::move(cond), std::move(block),
                                    std::move(els));
  } else {
    auto els = ParseBlock();
    if (!els) return nullptr;
    return std::make_unique<IfStmt>(pos, std::move(cond), std::move(block),
                                    std::move(els));
  }
}

std::unique_ptr<Block> Parser::ParseBlock() {
  if (Peek()->kind != TokenKind::LBRACE) {
    Raise("expected %s", ToString(TokenKind::LBRACE).c_str());
    return nullptr;
  }
  Pos pos = Bump()->pos;

  auto block = std::make_unique<Block>(pos);
  while (true) {
    if (Peek()->kind == TokenKind::RBRACE) {
      break;
    }
    auto stmt = ParseStmt();
    if (!stmt) return nullptr;
    block->stmts.push_back(std::move(stmt));
    if (Peek()->kind == TokenKind::RBRACE) {
      break;
    }
    if (Peek()->kind == TokenKind::SEMI) {
      Bump();
    }
  }
  Bump();
  return block;
}

template <typename... Args>
void Parser::Raise(const std::string& fmt, Args... args) {
  /* handler_.Raise(Peek()->pos, format(fmt, args...)); */
}

std::unique_ptr<File> Parser::Parse() {
  auto file = std::make_unique<File>();

  bool needs_nl = false;
  while (Peek()->kind != TokenKind::END) {
    if (needs_nl) {
      if (Peek()->kind == TokenKind::SEMI) {
        Bump();
      } else if (Peek()->nl) {
        // nothing
      } else {
        Raise("needs \\n or ;");
        return nullptr;
      }
    }
    if (Peek()->kind == TokenKind::KW_EXT) {
      auto ext = ParseExtern();
      if (!ext) return nullptr;
      file->externs.push_back(std::move(ext));
    } else if (Peek()->kind == TokenKind::KW_FN) {
      auto fn = ParseFnDecl();
      if (!fn) return nullptr;
      file->fnDecls.push_back(std::move(fn));
    } else {
      Raise("unknown top-level token");
      return nullptr;
    }
    needs_nl = true;
  }
  return file;
}

}  // namespace felis
