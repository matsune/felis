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

ParseResult<Extern> Parser::ParseExtern() {
  auto ext = Bump();
  assert(ext->kind == TokenKind::KW_EXT);
  auto pos = ext->pos;
  auto protoRes = ParseFnProto();
  if (!protoRes) return protoRes.Raise<Extern>();
  return ParseResult<Extern>::Ok(nextId_++, pos, protoRes.Unwrap());
}

ParseResult<FnDecl> Parser::ParseFnDecl() {
  auto protoRes = ParseFnProto();
  if (!protoRes) return protoRes.Raise<FnDecl>();
  auto blockRes = ParseBlock();
  if (!blockRes) return blockRes.Raise<FnDecl>();
  return ParseResult<FnDecl>::Ok(nextId_++, protoRes.Unwrap(),
                                 blockRes.Unwrap());
}

ParseResult<FnProto> Parser::ParseFnProto() {
  if (Peek()->kind != TokenKind::KW_FN) {
    return Raise<FnProto>("expected 'fn'");
  }
  auto fn = Bump();
  Pos fnPos = fn->pos;

  if (Peek()->kind != TokenKind::IDENT) {
    return Raise<FnProto>("expected ident");
  }
  auto nameTok = Bump();

  auto fnArgsRes = ParseFnArgs();
  if (!fnArgsRes) {
    return fnArgsRes.Raise<FnProto>();
  }

  if (Peek()->kind == TokenKind::ARROW) {
    Bump();
    if (Peek()->kind != TokenKind::IDENT) {
      return Raise<FnProto>("expected ident");
    }
    auto fnArgs = fnArgsRes.Unwrap();
    auto name = new Ident(nameTok->pos, nameTok->sval);
    auto tyTok = Bump();
    auto ty = new Ident(tyTok->pos, tyTok->sval);
    return ParseResult<FnProto>::Ok(fnPos, name, fnArgs, ty);
  } else {
    auto fnArgs = fnArgsRes.Unwrap();
    auto name = new Ident(nameTok->pos, nameTok->sval);
    return ParseResult<FnProto>::Ok(fnPos, name, fnArgs);
  }
}

// start with '('
ParseResult<FnArgs> Parser::ParseFnArgs() {
  if (Peek()->kind != TokenKind::LPAREN) {
    return Raise<FnArgs>("expected (");
  }
  Bump();

  if (Peek()->kind == TokenKind::RPAREN) {
    Bump();
    return ParseResult<FnArgs>::Ok();
  }

  auto argRes = ParseFnArg();
  if (!argRes) return argRes.Raise<FnArgs>();
  auto arg = argRes.Unwrap();
  bool hasName = arg->withName();
  auto args = new FnArgs;
  args->emplace_back(arg);

  while (Peek()->kind == TokenKind::COMMA) {
    Bump();

    auto argRes = ParseFnArg();
    if (!argRes) {
      delete args;
      return argRes.Raise<FnArgs>();
    }
    if (hasName != arg->withName()) {
      delete args;
      return Raise<FnArgs>("mixed name in func args");
    }
    args->emplace_back(argRes.Unwrap());
  }

  if (Peek()->kind != TokenKind::RPAREN) {
    delete args;
    return Raise<FnArgs>("expected )");
  }
  Bump();

  return ParseResult<FnArgs>::Ok(args);
}

ParseResult<FnArg> Parser::ParseFnArg() {
  if (Peek()->kind != TokenKind::IDENT) {
    return Raise<FnArg>("expected ident");
  }
  auto nameOrTyTok = Bump();

  if (Peek()->kind == TokenKind::COLON) {
    Bump();

    if (Peek()->kind != TokenKind::IDENT) {
      return Raise<FnArg>("expected ident");
    }

    auto name = new Ident(nameOrTyTok->pos, nameOrTyTok->sval);
    auto tyTok = Bump();
    return ParseResult<FnArg>::Ok(new Ident(tyTok->pos, tyTok->sval), name);
  } else {
    return ParseResult<FnArg>::Ok(
        new Ident(nameOrTyTok->pos, nameOrTyTok->sval));
  }
}

ParseResult<Expr> Parser::ParseExpr(uint8_t prec) {
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
    return Raise<Expr>("expected primary expr");
  }

  auto lhsRes = ParsePrimary();
  if (!lhsRes) return lhsRes;
  Expr* lhs = lhsRes.Unwrap();

  if (Peek()->kind == TokenKind::LPAREN) {
    if (lhs->ExprKind() != Expr::Kind::IDENT) {
      delete lhs;
      return Raise<Expr>("cannot call non function");
    }
    if (Peek()->kind != TokenKind::LPAREN) {
      delete lhs;
      return Raise<Expr>("expected %s", ToString(TokenKind::LPAREN).c_str());
    }
    Bump();

    std::vector<std::unique_ptr<Expr>> args;
    if (Peek()->kind == TokenKind::RPAREN) {
      Bump();
    } else {
      auto firstRes = ParseExpr();
      if (!firstRes) {
        delete lhs;
        return firstRes;
      }
      args.emplace_back(firstRes.Unwrap());

      if (Peek()->kind == TokenKind::RPAREN) {
        auto callIdent = (Ident*)lhs;
        auto call = new CallExpr(callIdent, std::move(args));
        return ParseResult<CallExpr>::Ok(call).Into<Expr>();
      }

      while (Peek()->kind != TokenKind::RPAREN) {
        if (Peek()->kind != TokenKind::COMMA) {
          delete lhs;
          return Raise<Expr>("expected %s", ToString(TokenKind::COMMA).c_str());
        }
        Bump();

        auto argRes = ParseExpr();
        if (!argRes) {
          delete lhs;
          return argRes;
        }
        args.emplace_back(argRes.Unwrap());
      }
      Bump();
    }
    lhs = new CallExpr((Ident*)lhs, std::move(args));
  }
  if (unOp) {
    return ParseResult<UnaryExpr>::Ok(pos, *unOp, lhs).Into<Expr>();
  }

  if (Peek()->nl) {
    return ParseResult<Expr>::Ok(lhs);
  }

  while (true) {
    if (!is_bin_op(Peek()->kind)) {
      return ParseResult<Expr>::Ok(lhs);
    }
    BinOp binOp(binOpFrom(Peek()->kind));
    if (binOp <= prec) {
      return ParseResult<Expr>::Ok(lhs);
    }

    Bump();
    auto rhsRes = ParseExpr(binOp);
    if (!rhsRes) {
      delete lhs;
      return rhsRes;
    }
    lhs = new BinaryExpr(std::unique_ptr<Expr>(lhs), binOp,
                         std::unique_ptr<Expr>(rhsRes.Unwrap()));
  }
}

ParseResult<Expr> Parser::ParsePrimary() {
  auto token = Bump();
  Pos pos = token->pos;
  if (token->kind == TokenKind::IDENT) {
    return ParseResult<Ident>::Ok(pos, token->sval).Into<Expr>();
  } else if (token->kind == TokenKind::LIT_INT) {
    return ParseResult<LitInt>::Ok(pos, token->ival).Into<Expr>();
  } else if (token->kind == TokenKind::LIT_FLOAT) {
    return ParseResult<LitFloat>::Ok(pos, token->fval).Into<Expr>();
  } else if (token->kind == TokenKind::LIT_BOOL) {
    return ParseResult<LitBool>::Ok(pos, token->bval).Into<Expr>();
  } else if (token->kind == TokenKind::LIT_CHAR) {
    return ParseResult<LitChar>::Ok(pos, token->cval).Into<Expr>();
  } else if (token->kind == TokenKind::LIT_STR) {
    return ParseResult<LitStr>::Ok(pos, token->sval).Into<Expr>();
  } else if (token->kind == TokenKind::LPAREN) {
    auto res = ParseExpr();
    if (!res) return res;
    if (Peek()->kind != TokenKind::RPAREN) {
      return Raise<Expr>("expected %s", ToString(TokenKind::RPAREN).c_str());
    }
    Bump();
    return res;
  }
  std::fprintf(stderr, "parsing unknown primary");
  std::terminate();
}

ParseResult<Stmt> Parser::ParseStmt() {
  if (Peek()->kind == TokenKind::KW_RET) {
    // Ret
    Pos pos = Bump()->pos;
    if (Peek()->kind == TokenKind::SEMI) {
      Bump();
      return ParseResult<RetStmt>::Ok(pos).Into<Stmt>();
    }
    if (Peek()->nl) {
      return ParseResult<RetStmt>::Ok(pos).Into<Stmt>();
    }
    auto exprRes = ParseExpr();
    if (!exprRes) return exprRes.Raise<Stmt>();
    return ParseResult<RetStmt>::Ok(pos, exprRes.Unwrap()).Into<Stmt>();

  } else if (Peek()->kind == TokenKind::KW_LET ||
             Peek()->kind == TokenKind::KW_VAR) {
    // variable decl
    auto kw = Bump();
    bool isLet = kw->kind == TokenKind::KW_LET;
    Pos pos = kw->pos;

    if (Peek()->kind != TokenKind::IDENT) {
      return Raise<Stmt>("expected %s", ToString(TokenKind::IDENT).c_str());
    }
    auto nameStr = Bump()->sval;
    /* auto name = new Ident(pos, Bump()->sval); */

    if (Peek()->kind != TokenKind::EQ) {
      return Raise<Stmt>("expected %s", ToString(TokenKind::EQ).c_str());
    }
    Bump();

    auto exprRes = ParseExpr();
    if (!exprRes) return exprRes.Raise<Stmt>();

    return ParseResult<VarDeclStmt>::Ok(pos, isLet, new Ident(pos, nameStr),
                                        exprRes.Unwrap())
        .Into<Stmt>();
  } else if (Peek()->kind == TokenKind::KW_IF) {
    return ParseIfStmt().Into<Stmt>();
  } else if (Peek()->kind == TokenKind::IDENT) {
    if (Peek2()->kind == TokenKind::EQ) {
      // assign stmt
      auto bump = Bump();
      auto name = new Ident(bump->pos, bump->sval);
      Bump();
      auto exprRes = ParseExpr();
      if (!exprRes) return exprRes.Raise<Stmt>();
      return ParseResult<AssignStmt>::Ok(name, exprRes.Unwrap()).Into<Stmt>();
    }
  }
  auto exprRes = ParseExpr();
  if (!exprRes) return exprRes.Raise<Stmt>();
  return exprRes.Into<Stmt>();
}

ParseResult<IfStmt> Parser::ParseIfStmt() {
  if (Peek()->kind != TokenKind::KW_IF) {
    return Raise<IfStmt>("expected 'if'");
  }
  Pos pos = Bump()->pos;

  auto condRes = ParseExpr();
  if (!condRes) return condRes.Raise<IfStmt>();
  auto blockRes = ParseBlock();
  if (!blockRes) return blockRes.Raise<IfStmt>();

  if (Peek()->kind != TokenKind::KW_ELSE) {
    return ParseResult<IfStmt>::Ok(pos, condRes.Unwrap(), blockRes.Unwrap());
  }
  Bump();

  if (Peek()->kind == TokenKind::KW_IF) {
    auto elsRes = ParseIfStmt();
    if (!elsRes) return elsRes;
    return ParseResult<IfStmt>::Ok(pos, condRes.Unwrap(), blockRes.Unwrap(),
                                   elsRes.Unwrap());
  } else {
    auto elsRes = ParseBlock();
    if (!elsRes) return elsRes.Raise<IfStmt>();
    return ParseResult<IfStmt>::Ok(pos, condRes.Unwrap(), blockRes.Unwrap(),
                                   elsRes.Unwrap());
  }
}

ParseResult<Block> Parser::ParseBlock() {
  if (Peek()->kind != TokenKind::LBRACE) {
    return Raise<Block>("expected %s", ToString(TokenKind::LBRACE).c_str());
  }
  Pos pos = Bump()->pos;

  auto block = new Block(pos);
  while (true) {
    if (Peek()->kind == TokenKind::RBRACE) {
      break;
    }
    auto stmtRes = ParseStmt();
    if (!stmtRes) {
      delete block;
      return stmtRes.Raise<Block>();
    }
    block->stmts.emplace_back(stmtRes.Unwrap());
    if (Peek()->kind == TokenKind::RBRACE) {
      break;
    }
    if (Peek()->kind == TokenKind::SEMI) {
      Bump();
    }
  }
  Bump();
  return ParseResult<Block>::Ok(block);
}

ParseResult<File> Parser::Parse() {
  auto file = new File;

  bool needs_nl = false;
  while (Peek()->kind != TokenKind::END) {
    if (needs_nl) {
      if (Peek()->kind == TokenKind::SEMI) {
        Bump();
      } else if (Peek()->nl) {
        // nothing
      } else {
        delete file;
        return Raise<File>("needs \\n or ;");
      }
    }
    if (Peek()->kind == TokenKind::KW_EXT) {
      auto extRes = ParseExtern();
      if (!extRes) {
        delete file;
        return extRes.Raise<File>();
      }
      file->externs.emplace_back(extRes.Unwrap());
    } else if (Peek()->kind == TokenKind::KW_FN) {
      auto fnRes = ParseFnDecl();
      if (!fnRes) {
        delete file;
        return fnRes.Raise<File>();
      }
      file->fnDecls.emplace_back(fnRes.Unwrap());
    } else {
      return Raise<File>("unknown top-level token");
    }
    needs_nl = true;
  }
  return ParseResult<File>::Ok(file);
}

template <typename T, typename... Args>
ParseResult<T> Parser::Raise(const std::string& fmt, Args... args) {
  return ParseResult<T>::Err(Peek()->pos, format(fmt, args...));
}

}  // namespace felis
