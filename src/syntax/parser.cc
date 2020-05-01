#include "syntax/parser.h"

#include <cassert>
#include <map>
#include <vector>

namespace felis {

namespace {

inline bool is_lit(Token::Kind kind) {
  return kind == Token::Kind::LIT_INT || kind == Token::Kind::LIT_FLOAT ||
         kind == Token::Kind::LIT_STR || kind == Token::Kind::LIT_CHAR ||
         kind == Token::Kind::LIT_BOOL;
}

inline bool is_primary(Token::Kind kind) {
  return kind == Token::Kind::LPAREN || kind == Token::Kind::IDENT ||
         is_lit(kind);
}

bool is_bin_op(Token::Kind kind) {
  switch (kind) {
    case Token::Kind::PLUS:
    case Token::Kind::MINUS:
    case Token::Kind::STAR:
    case Token::Kind::SLASH:
    case Token::Kind::PERCENT:
    case Token::Kind::LT:
    case Token::Kind::LE:
    case Token::Kind::GT:
    case Token::Kind::GE:
      return true;
    default:
      return false;
  }
}

inline ast::BinOp binOpFrom(Token::Kind kind) {
  switch (kind) {
    case Token::Kind::PLUS:
      return ast::BinOp::ADD;
    case Token::Kind::MINUS:
      return ast::BinOp::SUB;
    case Token::Kind::STAR:
      return ast::BinOp::MUL;
    case Token::Kind::SLASH:
      return ast::BinOp::DIV;
    case Token::Kind::PERCENT:
      return ast::BinOp::MOD;
    case Token::Kind::GT:
      return ast::BinOp::GT;
    case Token::Kind::GE:
      return ast::BinOp::GE;
    case Token::Kind::LT:
      return ast::BinOp::LT;
    case Token::Kind::LE:
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
  assert(ext->kind == Token::Kind::KW_EXT);
  auto pos = ext->pos;
  auto proto = ParseFnProto();
  return std::make_unique<ast::Extern>(pos, std::move(proto));
}

std::unique_ptr<ast::FnDecl> Parser::ParseFnDecl() {
  auto proto = ParseFnProto();
  auto block = ParseBlock();
  return std::make_unique<ast::FnDecl>(std::move(proto), std::move(block));
}

std::unique_ptr<ast::FnProto> Parser::ParseFnProto() {
  if (Peek()->kind != Token::Kind::KW_FN) {
    Throw("expected 'fn'");
  }
  auto fn = Bump();
  Pos fnPos = fn->pos;

  if (Peek()->kind != Token::Kind::IDENT) {
    Throw("expected ident");
  }
  auto nameTok = Bump();
  auto name = std::make_unique<ast::Ident>(nameTok->pos, nameTok->val);

  auto fnArgs = ParseFnArgs();

  if (Peek()->kind == Token::Kind::ARROW) {
    Bump();
    if (Peek()->kind != Token::Kind::IDENT) {
      Throw("expected ident");
    }
    auto tyTok = Bump();
    auto ty = std::make_unique<ast::Ident>(tyTok->pos, tyTok->val);
    return std::make_unique<ast::FnProto>(fnPos, std::move(name),
                                          std::move(fnArgs), std::move(ty));
  } else {
    return std::make_unique<ast::FnProto>(fnPos, std::move(name),
                                          std::move(fnArgs));
  }
}

// start with '('
std::vector<std::unique_ptr<ast::FnArg>> Parser::ParseFnArgs() {
  if (Peek()->kind != Token::Kind::LPAREN) {
    Throw("expected (");
  }
  Bump();

  std::vector<std::unique_ptr<ast::FnArg>> args;
  if (Peek()->kind == Token::Kind::RPAREN) {
    Bump();
    return args;
  }

  auto arg = ParseFnArg();
  bool hasName = arg->withName();
  std::map<std::string, bool> nameMap;
  if (hasName) {
    nameMap[arg->name->val] = true;
  }
  args.push_back(std::move(arg));

  while (Peek()->kind == Token::Kind::COMMA) {
    Bump();

    arg = ParseFnArg();
    if (hasName != arg->withName()) {
      Throw("mixed name in func args");
    }
    if (hasName) {
      if (nameMap.count(arg->name->val)) {
        Throw("duplicate argument %s", arg->name->val.c_str());
      }
      nameMap[arg->name->val] = true;
    }
    args.push_back(std::move(arg));
  }

  if (Peek()->kind != Token::Kind::RPAREN) {
    Throw("expected )");
  }
  Bump();

  return args;
}

std::unique_ptr<ast::FnArg> Parser::ParseFnArg() {
  if (Peek()->kind != Token::Kind::IDENT) {
    Throw("expected ident");
  }
  auto nameOrTyTok = Bump();

  if (Peek()->kind == Token::Kind::COLON) {
    Bump();

    if (Peek()->kind != Token::Kind::IDENT) {
      Throw("expected ident");
    }

    auto name =
        std::make_unique<ast::Ident>(nameOrTyTok->pos, nameOrTyTok->val);
    auto tyTok = Bump();
    return std::make_unique<ast::FnArg>(
        std::make_unique<ast::Ident>(tyTok->pos, tyTok->val), std::move(name));
  } else {
    return std::make_unique<ast::FnArg>(
        std::make_unique<ast::Ident>(nameOrTyTok->pos, nameOrTyTok->val));
  }
}

ast::Expr* Parser::ParseExpr(uint8_t prec) {
  std::unique_ptr<ast::UnOp> unOp;
  Pos pos;
  if (Peek()->kind == Token::Kind::MINUS) {
    pos = Bump()->pos;
    unOp = std::make_unique<ast::UnOp>(ast::UnOp::NEG);
  } else if (Peek()->kind == Token::Kind::NOT) {
    pos = Bump()->pos;
    unOp = std::make_unique<ast::UnOp>(ast::UnOp::NOT);
  }

  if (!is_primary(Peek()->kind)) {
    Throw("expected primary expr");
  }

  ast::Expr* lhs = ParsePrimary();

  if (Peek()->kind == Token::Kind::LPAREN) {
    if (lhs->ExprKind() != ast::Expr::Kind::IDENT) {
      delete lhs;
      Throw("cannot call non function");
    }
    if (Peek()->kind != Token::Kind::LPAREN) {
      delete lhs;
      Throw("expected %s", ToString(Token::Kind::LPAREN).c_str());
    }
    Bump();

    std::vector<std::unique_ptr<ast::Expr>> args;
    if (Peek()->kind == Token::Kind::RPAREN) {
      Bump();
    } else {
      try {
        auto first = ParseExpr();
        args.emplace_back(first);
      } catch (const CompileError& e) {
        delete lhs;
        throw e;
      }

      if (Peek()->kind == Token::Kind::RPAREN) {
        return new ast::CallExpr(
            std::unique_ptr<ast::Ident>(dynamic_cast<ast::Ident*>(lhs)),
            std::move(args));
      }

      while (Peek()->kind != Token::Kind::RPAREN) {
        if (Peek()->kind != Token::Kind::COMMA) {
          delete lhs;
          Throw("expected %s", ToString(Token::Kind::COMMA).c_str());
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
    lhs = new ast::CallExpr(std::unique_ptr<ast::Ident>((ast::Ident*)lhs),
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
  if (token->kind == Token::Kind::IDENT) {
    return new ast::Ident(pos, token->val);
  } else if (token->kind == Token::Kind::LIT_INT) {
    return new ast::Lit(pos, ast::Lit::Kind::INT, token->val);
  } else if (token->kind == Token::Kind::LIT_FLOAT) {
    return new ast::Lit(pos, ast::Lit::Kind::FLOAT, token->val);
  } else if (token->kind == Token::Kind::LIT_BOOL) {
    return new ast::Lit(pos, ast::Lit::Kind::BOOL, token->val);
  } else if (token->kind == Token::Kind::LIT_CHAR) {
    return new ast::Lit(pos, ast::Lit::Kind::CHAR, token->val);
  } else if (token->kind == Token::Kind::LIT_STR) {
    return new ast::Lit(pos, ast::Lit::Kind::STRING, token->val);
  } else if (token->kind == Token::Kind::LPAREN) {
    auto expr = ParseExpr();
    if (Peek()->kind != Token::Kind::RPAREN) {
      delete expr;
      Throw("expected %s", ToString(Token::Kind::RPAREN).c_str());
    }
    Bump();
    return expr;
  }
  std::fprintf(stderr, "parsing unknown primary");
  std::terminate();
}

std::unique_ptr<ast::Stmt> Parser::ParseStmt() {
  if (Peek()->kind == Token::Kind::KW_RET) {
    // Ret
    Pos pos = Bump()->pos;
    if (Peek()->kind == Token::Kind::SEMI) {
      Bump();
      return std::make_unique<ast::RetStmt>(pos);
    }
    if (Peek()->nl) {
      return std::make_unique<ast::RetStmt>(pos);
    }
    auto expr = ParseExpr();
    return std::make_unique<ast::RetStmt>(pos, expr);

  } else if (Peek()->kind == Token::Kind::KW_LET ||
             Peek()->kind == Token::Kind::KW_VAR) {
    // variable decl
    auto kw = Bump();
    bool isLet = kw->kind == Token::Kind::KW_LET;
    Pos pos = kw->pos;

    if (Peek()->kind != Token::Kind::IDENT) {
      Throw("expected %s", ToString(Token::Kind::IDENT).c_str());
    }
    auto name = std::make_unique<ast::Ident>(pos, Bump()->val);

    if (Peek()->kind != Token::Kind::EQ) {
      Throw("expected %s", ToString(Token::Kind::EQ).c_str());
    }
    Bump();

    auto expr = ParseExpr();
    return std::make_unique<ast::VarDeclStmt>(pos, isLet, std::move(name),
                                              expr);
  } else if (Peek()->kind == Token::Kind::KW_IF) {
    return ParseIfStmt();
  } else if (Peek()->kind == Token::Kind::IDENT) {
    if (Peek2()->kind == Token::Kind::EQ) {
      // assign stmt
      auto bump = Bump();
      auto name = std::make_unique<ast::Ident>(bump->pos, bump->val);
      Bump();
      auto expr = ParseExpr();
      return std::make_unique<ast::AssignStmt>(std::move(name), expr);
    }
  }
  auto expr = ParseExpr();
  return std::unique_ptr<ast::Expr>(expr);
}

std::unique_ptr<ast::IfStmt> Parser::ParseIfStmt() {
  if (Peek()->kind != Token::Kind::KW_IF) {
    Throw("expected 'if'");
  }
  Pos pos = Bump()->pos;

  auto cond = ParseExpr();
  try {
    auto block = ParseBlock();
    if (Peek()->kind != Token::Kind::KW_ELSE) {
      return std::make_unique<ast::IfStmt>(pos, cond, std::move(block));
    }
    Bump();

    if (Peek()->kind == Token::Kind::KW_IF) {
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
  if (Peek()->kind != Token::Kind::LBRACE) {
    Throw("expected %s", ToString(Token::Kind::LBRACE).c_str());
  }
  Pos pos = Bump()->pos;

  auto block = std::make_unique<ast::Block>(pos);
  while (true) {
    if (Peek()->kind == Token::Kind::RBRACE) {
      break;
    }
    auto stmt = ParseStmt();
    block->stmts.push_back(std::move(stmt));
    if (Peek()->kind == Token::Kind::RBRACE) {
      break;
    }
    if (Peek()->kind == Token::Kind::SEMI) {
      Bump();
    }
  }
  Bump();
  return block;
}

std::unique_ptr<ast::File> Parser::Parse() {
  auto file = std::make_unique<ast::File>();

  bool needs_nl = false;
  while (Peek()->kind != Token::Kind::END) {
    if (needs_nl) {
      if (Peek()->kind == Token::Kind::SEMI) {
        Bump();
      } else if (Peek()->nl) {
        // nothing
      } else {
        Throw("needs \\n or ;");
      }
    }
    if (Peek()->kind == Token::Kind::KW_EXT) {
      auto ext = ParseExtern();
      file->externs.push_back(std::move(ext));
    } else if (Peek()->kind == Token::Kind::KW_FN) {
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
  throw CompileError::CreatePos(Peek()->pos, fmt, args...);
}

}  // namespace felis
