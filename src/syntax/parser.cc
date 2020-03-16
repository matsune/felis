#include "syntax/parser.h"

#include <cassert>
#include <vector>

#include "string/string.h"

#define EXPECT(KIND)                               \
  if (Peek()->kind != KIND) {                      \
    Raise("expected %s", to_string(KIND).c_str()); \
    return nullptr;                                \
  }

namespace felis {

namespace {

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
  assert(Bump()->kind == TokenKind::KW_EXT);
  auto proto = ParseFnProto();
  if (!proto) return nullptr;
  return std::make_unique<Extern>(std::move(proto));
}

std::unique_ptr<FnDecl> Parser::ParseFnDecl() {
  auto proto = ParseFnProto();
  if (!proto) return nullptr;
  auto block = ParseBlock();
  if (!block) return nullptr;
  return std::make_unique<FnDecl>(std::move(proto), std::move(block));
}

std::unique_ptr<FnProto> Parser::ParseFnProto() {
  EXPECT(TokenKind::KW_FN);
  Bump();

  EXPECT(TokenKind::IDENT);
  auto name = std::make_unique<Ident>(Bump()->sval);

  std::vector<std::unique_ptr<FnArg>> args;
  if (!ParseFnArgs(args)) return nullptr;

  if (Peek()->kind == TokenKind::ARROW) {
    Bump();
    if (Peek()->kind != TokenKind::IDENT) {
      Raise("expected ident");
      return nullptr;
    }
    auto ty = std::make_unique<Ident>(Bump()->sval);
    return std::make_unique<FnProto>(std::move(name), std::move(args),
                                     std::move(ty));
  } else {
    return std::make_unique<FnProto>(std::move(name), std::move(args));
  }
}

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
  EXPECT(TokenKind::IDENT);
  auto nameOrTy = std::make_unique<Ident>(Bump()->sval);

  if (Peek()->kind == TokenKind::COLON) {
    Bump();

    EXPECT(TokenKind::IDENT);

    auto ty = std::make_unique<Ident>(Bump()->sval);
    return std::make_unique<FnArg>(std::move(ty), std::move(nameOrTy));
  } else {
    return std::make_unique<FnArg>(std::move(nameOrTy));
  }
}

std::unique_ptr<Expr> Parser::ParseExpr(uint8_t prec) {
  std::unique_ptr<UnOp> unOp;
  if (Peek()->kind == TokenKind::MINUS) {
    Bump();
    unOp = std::make_unique<UnOp>(UnOp::NEG);
  } else if (Peek()->kind == TokenKind::NOT) {
    Bump();
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
    EXPECT(TokenKind::LPAREN);
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
        EXPECT(TokenKind::COMMA);
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
    return std::make_unique<UnaryExpr>(std::move(unOp), std::move(lhs));
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
  if (token->kind == TokenKind::IDENT) {
    return std::make_unique<Ident>(token->sval);
  } else if (token->kind == TokenKind::LIT_INT) {
    return std::make_unique<LitInt>(token->ival);
  } else if (token->kind == TokenKind::LIT_BOOL) {
    return std::make_unique<LitBool>(token->bval);
  } else if (token->kind == TokenKind::LIT_CHAR) {
    return std::make_unique<LitChar>(token->ival);
  } else if (token->kind == TokenKind::LIT_STR) {
    return std::make_unique<LitStr>(token->sval);
  } else if (token->kind == TokenKind::LPAREN) {
    auto expr = ParseExpr();
    if (!expr) return nullptr;
    EXPECT(TokenKind::RPAREN);
    Bump();
    return expr;
  }
  return nullptr;
}

std::unique_ptr<Stmt> Parser::ParseStmt() {
  if (Peek()->kind == TokenKind::KW_RET) {
    Bump();
    if (Peek()->kind == TokenKind::RBRACE || Peek()->kind == TokenKind::SEMI) {
      Bump();
      return std::make_unique<RetStmt>();
    }
    auto expr = ParseExpr();
    if (!expr) return nullptr;
    return std::make_unique<RetStmt>(std::move(expr));
  } else if (Peek()->kind == TokenKind::KW_LET ||
             Peek()->kind == TokenKind::KW_VAR) {
    bool isLet = Bump()->kind == TokenKind::KW_LET;

    EXPECT(TokenKind::IDENT);
    auto name = std::make_unique<Ident>(Bump()->sval);

    EXPECT(TokenKind::EQ);
    Bump();

    auto expr = ParseExpr();
    if (!expr) return nullptr;

    return std::make_unique<VarDeclStmt>(isLet, std::move(name),
                                         std::move(expr));
  } else if (Peek()->kind == TokenKind::KW_IF) {
    return ParseIfStmt();
  } else if (Peek()->kind == TokenKind::IDENT) {
    if (Peek2()->kind == TokenKind::EQ) {
      // assign stmt
      auto name = std::make_unique<Ident>(Bump()->sval);
      Bump();
      auto expr = ParseExpr();
      if (!expr) return nullptr;
      return std::make_unique<AssignStmt>(std::move(name), std::move(expr));
    }
  }
  return ParseExpr();
}

std::unique_ptr<IfStmt> Parser::ParseIfStmt() {
  EXPECT(TokenKind::KW_IF);
  Bump();

  auto cond = ParseExpr();
  if (!cond) return nullptr;
  auto block = ParseBlock();
  if (!block) return nullptr;

  if (Peek()->kind != TokenKind::KW_ELSE) {
    return std::make_unique<IfStmt>(std::move(cond), std::move(block));
  }
  Bump();

  if (Peek()->kind == TokenKind::KW_IF) {
    auto els = ParseIfStmt();
    if (!els) return nullptr;
    return std::make_unique<IfStmt>(std::move(cond), std::move(block),
                                    std::move(els));
  } else {
    auto els = ParseBlock();
    if (!els) return nullptr;
    return std::make_unique<IfStmt>(std::move(cond), std::move(block),
                                    std::move(els));
  }
}

std::unique_ptr<Block> Parser::ParseBlock() {
  EXPECT(TokenKind::LBRACE);
  Bump();

  auto block = std::make_unique<Block>();
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
  handler_.Raise(Peek()->pos, format(fmt, args...));
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
