#include "syntax/parser.h"

#include <cassert>
#include <map>
#include <vector>

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

inline ast::BinOp binOpFrom(TokenKind kind) {
  switch (kind) {
    case TokenKind::PLUS:
      return ast::BinOp::ADD;
    case TokenKind::MINUS:
      return ast::BinOp::SUB;
    case TokenKind::STAR:
      return ast::BinOp::MUL;
    case TokenKind::SLASH:
      return ast::BinOp::DIV;
    case TokenKind::PERCENT:
      return ast::BinOp::MOD;
    case TokenKind::GT:
      return ast::BinOp::GT;
    case TokenKind::GE:
      return ast::BinOp::GE;
    case TokenKind::LT:
      return ast::BinOp::LT;
    case TokenKind::LE:
      return ast::BinOp::LE;
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

std::unique_ptr<ast::Extern> Parser::ParseExtern() {
  auto ext = Bump();
  assert(ext->kind == TokenKind::KW_EXT);
  auto pos = ext->pos;
  auto proto = ParseFnProto();
  return std::make_unique<ast::Extern>(nextId_++, pos, std::move(proto));
}

std::unique_ptr<ast::FnDecl> Parser::ParseFnDecl() {
  auto proto = ParseFnProto();
  auto block = ParseBlock();
  return std::make_unique<ast::FnDecl>(nextId_++, std::move(proto),
                                       std::move(block));
}

std::unique_ptr<ast::FnProto> Parser::ParseFnProto() {
  if (Peek()->kind != TokenKind::KW_FN) {
    Throw("expected 'fn'");
  }
  auto fn = Bump();
  Pos fnPos = fn->pos;

  if (Peek()->kind != TokenKind::IDENT) {
    Throw("expected ident");
  }
  auto nameTok = Bump();
  auto name = std::make_unique<ast::Ident>(nameTok->pos, nameTok->sval);

  auto fnArgs = ParseFnArgs();

  if (Peek()->kind == TokenKind::ARROW) {
    Bump();
    if (Peek()->kind != TokenKind::IDENT) {
      Throw("expected ident");
    }
    auto tyTok = Bump();
    auto ty = std::make_unique<ast::Ident>(tyTok->pos, tyTok->sval);
    return std::make_unique<ast::FnProto>(fnPos, std::move(name),
                                          std::move(fnArgs), std::move(ty));
  } else {
    return std::make_unique<ast::FnProto>(fnPos, std::move(name),
                                          std::move(fnArgs));
  }
}

// start with '('
std::vector<std::unique_ptr<ast::FnArg>> Parser::ParseFnArgs() {
  if (Peek()->kind != TokenKind::LPAREN) {
    Throw("expected (");
  }
  Bump();

  std::vector<std::unique_ptr<ast::FnArg>> args;
  if (Peek()->kind == TokenKind::RPAREN) {
    Bump();
    return args;
  }

  auto arg = ParseFnArg();
  bool hasName = arg->withName();
  std::map<std::string, bool> nameMap;
  if (hasName) {
    nameMap[arg->name->sval] = true;
  }
  args.push_back(std::move(arg));

  while (Peek()->kind == TokenKind::COMMA) {
    Bump();

    arg = ParseFnArg();
    if (hasName != arg->withName()) {
      Throw("mixed name in func args");
    }
    if (hasName) {
      if (nameMap.count(arg->name->sval)) {
        Throw("duplicate argument %s", arg->name->sval.c_str());
      }
      nameMap[arg->name->sval] = true;
    }
    args.push_back(std::move(arg));
  }

  if (Peek()->kind != TokenKind::RPAREN) {
    Throw("expected )");
  }
  Bump();

  return args;
}

std::unique_ptr<ast::FnArg> Parser::ParseFnArg() {
  if (Peek()->kind != TokenKind::IDENT) {
    Throw("expected ident");
  }
  auto nameOrTyTok = Bump();

  if (Peek()->kind == TokenKind::COLON) {
    Bump();

    if (Peek()->kind != TokenKind::IDENT) {
      Throw("expected ident");
    }

    auto name =
        std::make_unique<ast::Ident>(nameOrTyTok->pos, nameOrTyTok->sval);
    auto tyTok = Bump();
    return std::make_unique<ast::FnArg>(
        std::make_unique<ast::Ident>(tyTok->pos, tyTok->sval), std::move(name));
  } else {
    return std::make_unique<ast::FnArg>(
        std::make_unique<ast::Ident>(nameOrTyTok->pos, nameOrTyTok->sval));
  }
}

ast::Expr* Parser::ParseExpr(uint8_t prec) {
  std::unique_ptr<ast::UnOp> unOp;
  Pos pos;
  if (Peek()->kind == TokenKind::MINUS) {
    pos = Bump()->pos;
    unOp = std::make_unique<ast::UnOp>(ast::UnOp::NEG);
  } else if (Peek()->kind == TokenKind::NOT) {
    pos = Bump()->pos;
    unOp = std::make_unique<ast::UnOp>(ast::UnOp::NOT);
  }

  if (!is_primary(Peek()->kind)) {
    Throw("expected primary expr");
  }

  ast::Expr* lhs = ParsePrimary();

  if (Peek()->kind == TokenKind::LPAREN) {
    if (lhs->ExprKind() != ast::Expr::Kind::IDENT) {
      delete lhs;
      Throw("cannot call non function");
    }
    if (Peek()->kind != TokenKind::LPAREN) {
      delete lhs;
      Throw("expected %s", ToString(TokenKind::LPAREN).c_str());
    }
    Bump();

    std::vector<std::unique_ptr<ast::Expr>> args;
    if (Peek()->kind == TokenKind::RPAREN) {
      Bump();
    } else {
      try {
        auto first = ParseExpr();
        args.emplace_back(first);
      } catch (const CompileError& e) {
        delete lhs;
        throw e;
      }

      if (Peek()->kind == TokenKind::RPAREN) {
        return new ast::CallExpr(
            std::unique_ptr<ast::Ident>(dynamic_cast<ast::Ident*>(lhs)),
            std::move(args));
      }

      while (Peek()->kind != TokenKind::RPAREN) {
        if (Peek()->kind != TokenKind::COMMA) {
          delete lhs;
          Throw("expected %s", ToString(TokenKind::COMMA).c_str());
        }
        Bump();

        try {
          ast::Expr* arg = ParseExpr();
          args.emplace_back(arg);
        } catch (const CompileError& e) {
          delete lhs;
          throw e;
        }
      }
      Bump();
    }
    lhs = new ast::CallExpr(
        std::unique_ptr<ast::Ident>(dynamic_cast<ast::Ident*>(lhs)),
        std::move(args));
  }
  if (unOp) {
    return new ast::UnaryExpr(pos, *unOp, lhs);
  }

  if (Peek()->nl) {
    return lhs;
  }

  while (true) {
    if (!is_bin_op(Peek()->kind)) {
      return lhs;
    }
    ast::BinOp binOp(binOpFrom(Peek()->kind));
    if (binOp <= prec) {
      return lhs;
    }

    Bump();
    try {
      ast::Expr* rhs = ParseExpr(binOp);
      lhs = new ast::BinaryExpr(lhs, binOp, rhs);
    } catch (const CompileError& e) {
      delete lhs;
      throw e;
    }
  }
}  // namespace felis

ast::Expr* Parser::ParsePrimary() {
  auto token = Bump();
  Pos pos = token->pos;
  if (token->kind == TokenKind::IDENT) {
    return new ast::Ident(pos, token->sval);
  } else if (token->kind == TokenKind::LIT_INT) {
    return new ast::LitInt(pos, token->ival);
  } else if (token->kind == TokenKind::LIT_FLOAT) {
    return new ast::LitFloat(pos, token->fval);
  } else if (token->kind == TokenKind::LIT_BOOL) {
    return new ast::LitBool(pos, token->bval);
  } else if (token->kind == TokenKind::LIT_CHAR) {
    return new ast::LitChar(pos, token->cval);
  } else if (token->kind == TokenKind::LIT_STR) {
    return new ast::LitStr(pos, token->sval);
  } else if (token->kind == TokenKind::LPAREN) {
    auto expr = ParseExpr();
    if (Peek()->kind != TokenKind::RPAREN) {
      delete expr;
      Throw("expected %s", ToString(TokenKind::RPAREN).c_str());
    }
    Bump();
    return expr;
  }
  std::fprintf(stderr, "parsing unknown primary");
  std::terminate();
}

std::unique_ptr<ast::Stmt> Parser::ParseStmt() {
  if (Peek()->kind == TokenKind::KW_RET) {
    // Ret
    Pos pos = Bump()->pos;
    if (Peek()->kind == TokenKind::SEMI) {
      Bump();
      return std::make_unique<ast::RetStmt>(pos);
    }
    if (Peek()->nl) {
      return std::make_unique<ast::RetStmt>(pos);
    }
    auto expr = ParseExpr();
    return std::make_unique<ast::RetStmt>(pos, expr);

  } else if (Peek()->kind == TokenKind::KW_LET ||
             Peek()->kind == TokenKind::KW_VAR) {
    // variable decl
    auto kw = Bump();
    bool isLet = kw->kind == TokenKind::KW_LET;
    Pos pos = kw->pos;

    if (Peek()->kind != TokenKind::IDENT) {
      Throw("expected %s", ToString(TokenKind::IDENT).c_str());
    }
    auto name = std::make_unique<ast::Ident>(pos, Bump()->sval);

    if (Peek()->kind != TokenKind::EQ) {
      Throw("expected %s", ToString(TokenKind::EQ).c_str());
    }
    Bump();

    auto expr = ParseExpr();
    return std::make_unique<ast::VarDeclStmt>(pos, isLet, std::move(name),
                                              expr);
  } else if (Peek()->kind == TokenKind::KW_IF) {
    return ParseIfStmt();
  } else if (Peek()->kind == TokenKind::IDENT) {
    if (Peek2()->kind == TokenKind::EQ) {
      // assign stmt
      auto bump = Bump();
      auto name = std::make_unique<ast::Ident>(bump->pos, bump->sval);
      Bump();
      auto expr = ParseExpr();
      return std::make_unique<ast::AssignStmt>(std::move(name), expr);
    }
  }
  auto expr = ParseExpr();
  return std::unique_ptr<ast::Expr>(expr);
}

std::unique_ptr<ast::IfStmt> Parser::ParseIfStmt() {
  if (Peek()->kind != TokenKind::KW_IF) {
    Throw("expected 'if'");
  }
  Pos pos = Bump()->pos;

  auto cond = ParseExpr();
  try {
    auto block = ParseBlock();
    if (Peek()->kind != TokenKind::KW_ELSE) {
      return std::make_unique<ast::IfStmt>(pos, cond, std::move(block));
    }
    Bump();

    if (Peek()->kind == TokenKind::KW_IF) {
      auto els = ParseIfStmt();
      return std::make_unique<ast::IfStmt>(pos, cond, std::move(block),
                                           std::move(els));
    } else {
      auto els = ParseBlock();
      return std::make_unique<ast::IfStmt>(pos, cond, std::move(block),
                                           std::move(els));
    }
  } catch (const CompileError& e) {
    delete cond;
    throw e;
  }
}

std::unique_ptr<ast::Block> Parser::ParseBlock() {
  if (Peek()->kind != TokenKind::LBRACE) {
    Throw("expected %s", ToString(TokenKind::LBRACE).c_str());
  }
  Pos pos = Bump()->pos;

  auto block = std::make_unique<ast::Block>(pos);
  while (true) {
    if (Peek()->kind == TokenKind::RBRACE) {
      break;
    }
    auto stmt = ParseStmt();
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

std::unique_ptr<ast::File> Parser::Parse() {
  auto file = std::make_unique<ast::File>();

  bool needs_nl = false;
  while (Peek()->kind != TokenKind::END) {
    if (needs_nl) {
      if (Peek()->kind == TokenKind::SEMI) {
        Bump();
      } else if (Peek()->nl) {
        // nothing
      } else {
        Throw("needs \\n or ;");
      }
    }
    if (Peek()->kind == TokenKind::KW_EXT) {
      auto ext = ParseExtern();
      file->externs.push_back(std::move(ext));
    } else if (Peek()->kind == TokenKind::KW_FN) {
      auto fn = ParseFnDecl();
      file->fnDecls.push_back(std::move(fn));
    } else {
      Throw("unknown top-level token");
    }
    needs_nl = true;
  }
  return std::move(file);
}

template <typename... Args>
void Parser::Throw(const std::string& fmt, Args... args) {
  throw CompileError::CreatePosFmt(Peek()->pos, fmt, args...);
}

}  // namespace felis
