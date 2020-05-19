#include "syntax/parser.h"

#include <cassert>
#include <map>
#include <vector>

#include "unique.h"

namespace felis {

namespace {

inline bool is_lit(Token::Kind kind) {
  return kind == Token::Kind::LIT_INT || kind == Token::Kind::LIT_FLOAT ||
         kind == Token::Kind::LIT_STR || kind == Token::Kind::LIT_CHAR ||
         kind == Token::Kind::LIT_BOOL;
}

inline bool is_primary(Token::Kind kind) {
  return kind == Token::Kind::LBRACE || kind == Token::Kind::KW_IF ||
         kind == Token::Kind::LPAREN || kind == Token::Kind::IDENT ||
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
    case Token::Kind::EQEQ:
    case Token::Kind::NEQ:
      return true;
    default:
      return false;
  }
}

}  // namespace

inline ast::BinaryOp::Op binop_from_tok(Token::Kind kind) {
  switch (kind) {
    case Token::Kind::PLUS:
      return ast::BinaryOp::Op::ADD;
    case Token::Kind::MINUS:
      return ast::BinaryOp::Op::SUB;
    case Token::Kind::STAR:
      return ast::BinaryOp::Op::MUL;
    case Token::Kind::SLASH:
      return ast::BinaryOp::Op::DIV;
    case Token::Kind::PERCENT:
      return ast::BinaryOp::Op::MOD;
    case Token::Kind::GT:
      return ast::BinaryOp::Op::GT;
    case Token::Kind::GE:
      return ast::BinaryOp::Op::GE;
    case Token::Kind::LT:
      return ast::BinaryOp::Op::LT;
    case Token::Kind::LE:
      return ast::BinaryOp::Op::LE;
    case Token::Kind::EQEQ:
      return ast::BinaryOp::EQEQ;
    case Token::Kind::NEQ:
      return ast::BinaryOp::NEQ;
    default:
      assert(false);
  }
}

const Token* Parser::Peek() const { return tokens_.front().get(); }

const Token* Parser::Peek2() const {
  if (tokens_.size() < 2) return nullptr;
  return tokens_[1].get();
}

std::unique_ptr<Token> Parser::Bump() { return tokens_.move_front(); }

std::unique_ptr<ast::Extern> Parser::ParseExtern() {
  auto ext = Bump();
  assert(ext->kind == Token::Kind::KW_EXT);
  auto begin = ext->begin;
  auto proto = ParseFnProto();
  return std::make_unique<ast::Extern>(begin, std::move(proto));
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
  Loc fn_begin = fn->begin;

  if (Peek()->kind != Token::Kind::IDENT) {
    Throw("expected ident");
  }
  auto name_tok = Bump();
  auto name = std::make_unique<ast::Ident>(name_tok->begin, name_tok->val);

  auto fn_args = ParseFnArgs();

  if (Peek()->kind == Token::Kind::ARROW) {
    Bump();
    if (Peek()->kind != Token::Kind::IDENT) {
      Throw("expected ident");
    }
    auto ty_tok = Bump();
    auto ty = std::make_unique<ast::Ident>(ty_tok->begin, ty_tok->val);
    return std::make_unique<ast::FnProto>(fn_begin, std::move(name),
                                          std::move(fn_args), std::move(ty));
  } else {
    return std::make_unique<ast::FnProto>(fn_begin, std::move(name),
                                          std::move(fn_args));
  }
}

// start with '('
std::unique_ptr<ast::FnArgs> Parser::ParseFnArgs() {
  if (Peek()->kind != Token::Kind::LPAREN) {
    Throw("expected (");
  }
  Loc begin = Bump()->begin;

  std::vector<std::unique_ptr<ast::FnArg>> args;
  if (Peek()->kind == Token::Kind::RPAREN) {
    Loc end = Bump()->begin;
    return std::make_unique<ast::FnArgs>(begin, end, std::move(args));
  }

  auto arg = ParseFnArg();
  bool has_name = arg->WithName();
  std::map<std::string, bool> name_map;
  if (has_name) {
    name_map[arg->name->val] = true;
  }
  args.push_back(std::move(arg));

  while (Peek()->kind == Token::Kind::COMMA) {
    Bump();

    arg = ParseFnArg();
    if (has_name != arg->WithName()) {
      Throw("mixed name in func args");
    }
    if (has_name) {
      if (name_map.count(arg->name->val)) {
        Throw("duplicate argument %s", arg->name->val.c_str());
      }
      name_map[arg->name->val] = true;
    }
    args.push_back(std::move(arg));
  }

  if (Peek()->kind != Token::Kind::RPAREN) {
    Throw("expected )");
  }
  Loc end = Bump()->begin;

  return std::make_unique<ast::FnArgs>(begin, end, std::move(args));
}

std::unique_ptr<ast::FnArg> Parser::ParseFnArg() {
  if (Peek()->kind != Token::Kind::IDENT) {
    Throw("expected ident");
  }
  auto name_or_ty_tok = Bump();

  if (Peek()->kind == Token::Kind::COLON) {
    Bump();

    if (Peek()->kind != Token::Kind::IDENT) {
      Throw("expected ident");
    }

    auto name = std::make_unique<ast::Ident>(name_or_ty_tok->begin,
                                             name_or_ty_tok->val);
    auto ty_tok = Bump();
    return std::make_unique<ast::FnArg>(
        std::make_unique<ast::Ident>(ty_tok->begin, ty_tok->val),
        std::move(name));
  } else {
    return std::make_unique<ast::FnArg>(std::make_unique<ast::Ident>(
        name_or_ty_tok->begin, name_or_ty_tok->val));
  }
}

std::unique_ptr<ast::Expr> Parser::ParseExpr(uint8_t prec) {
  std::unique_ptr<ast::UnaryOp> un_op;
  if (Peek()->kind == Token::Kind::MINUS) {
    Loc begin = Bump()->begin;
    un_op = std::make_unique<ast::UnaryOp>(begin, ast::UnaryOp::Op::NEG);
  } else if (Peek()->kind == Token::Kind::NOT) {
    Loc begin = Bump()->begin;
    un_op = std::make_unique<ast::UnaryOp>(begin, ast::UnaryOp::Op::NOT);
  }

  if (!is_primary(Peek()->kind)) {
    Throw("expected primary expr");
  }

  auto lhs = ParsePrimary();

  if (Peek()->kind == Token::Kind::LPAREN) {
    if (lhs->ExprKind() != ast::Expr::Kind::IDENT) {
      Throw("cannot call non function");
    }
    if (Peek()->kind != Token::Kind::LPAREN) {
      Throw("expected %s", ToString(Token::Kind::LPAREN).c_str());
    }
    Bump();

    unique_deque<ast::Expr> args;
    Loc end;
    if (Peek()->kind == Token::Kind::RPAREN) {
      end = Bump()->begin;
    } else {
      args.push_back(ParseExpr());

      if (Peek()->kind == Token::Kind::RPAREN) {
        end = Bump()->end;
        return std::make_unique<ast::CallExpr>(
            end, unique_cast<ast::Ident>(std::move(lhs)), std::move(args));
      }

      while (Peek()->kind != Token::Kind::RPAREN) {
        if (Peek()->kind != Token::Kind::COMMA) {
          Throw("expected %s", ToString(Token::Kind::COMMA).c_str());
        }
        Bump();

        args.push_back(ParseExpr());
      }
      end = Bump()->begin;
    }
    lhs = std::make_unique<ast::CallExpr>(
        end, unique_cast<ast::Ident>(std::move(lhs)), std::move(args));
  }
  if (un_op) {
    return std::make_unique<ast::UnaryExpr>(std::move(un_op), std::move(lhs));
  }

  if (Peek()->nl) {
    return lhs;
  }

  while (true) {
    if (!is_bin_op(Peek()->kind)) {
      return lhs;
    }
    auto peek = Peek();
    ast::BinaryOp::Op op = binop_from_tok(peek->kind);
    Loc begin = peek->begin;
    auto bin_op = std::make_unique<ast::BinaryOp>(begin, op);
    if (op <= prec) {
      return lhs;
    }

    Bump();
    lhs = std::make_unique<ast::BinaryExpr>(std::move(lhs), std::move(bin_op),
                                            ParseExpr(op));
  }
}  // namespace felis

std::unique_ptr<ast::Expr> Parser::ParsePrimary() {
  auto token = Peek();
  Loc begin = token->begin;
  switch (token->kind) {
    case Token::Kind::LBRACE:
      return ParseBlock();
    case Token::Kind::KW_IF:
      return ParseIf();
    case Token::Kind::IDENT:
      Bump();
      return std::make_unique<ast::Ident>(begin, token->val);
    case Token::Kind::LIT_INT:
      Bump();
      return std::make_unique<ast::Lit>(begin, ast::Lit::Kind::INT, token->val);
    case Token::Kind::LIT_FLOAT:
      Bump();
      return std::make_unique<ast::Lit>(begin, ast::Lit::Kind::FLOAT,
                                        token->val);
    case Token::Kind::LIT_BOOL:
      Bump();
      return std::make_unique<ast::Lit>(begin, ast::Lit::Kind::BOOL,
                                        token->val);
    case Token::Kind::LIT_CHAR:
      Bump();
      return std::make_unique<ast::Lit>(begin, ast::Lit::Kind::CHAR,
                                        token->val);
    case Token::Kind::LIT_STR:
      Bump();
      return std::make_unique<ast::Lit>(begin, ast::Lit::Kind::STRING,
                                        token->val);
    case Token::Kind::LPAREN: {
      Bump();
      auto expr = ParseExpr();
      if (Peek()->kind != Token::Kind::RPAREN) {
        Throw("expected %s", ToString(Token::Kind::RPAREN).c_str());
      }
      Bump();
      return expr;
    }
    default:
      std::fprintf(stderr, "parsing unknown primary");
      std::terminate();
  }
}

std::unique_ptr<ast::Stmt> Parser::ParseStmt() {
  if (Peek()->kind == Token::Kind::KW_RET) {
    // Ret
    Loc begin = Bump()->begin;
    if (Peek()->kind == Token::Kind::SEMI) {
      Bump();
      return std::make_unique<ast::RetStmt>(begin);
    }
    if (Peek()->nl) {
      return std::make_unique<ast::RetStmt>(begin);
    }
    return std::make_unique<ast::RetStmt>(begin, ParseExpr());

  } else if (Peek()->kind == Token::Kind::KW_LET ||
             Peek()->kind == Token::Kind::KW_VAR) {
    // variable decl
    auto kw = Bump();
    bool is_let = kw->kind == Token::Kind::KW_LET;
    Loc begin = kw->begin;

    if (Peek()->kind != Token::Kind::IDENT) {
      Throw("expected %s", ToString(Token::Kind::IDENT).c_str());
    }
    auto name = std::make_unique<ast::Ident>(begin, Bump()->val);

    if (Peek()->kind != Token::Kind::EQ) {
      Throw("expected %s", ToString(Token::Kind::EQ).c_str());
    }
    Bump();

    return std::make_unique<ast::VarDeclStmt>(begin, is_let, std::move(name),
                                              ParseExpr());
  } else if (Peek()->kind == Token::Kind::IDENT) {
    if (Peek2()->kind == Token::Kind::EQ) {
      // assign stmt
      auto bump = Bump();
      auto name = std::make_unique<ast::Ident>(bump->begin, bump->val);
      Bump();
      return std::make_unique<ast::AssignStmt>(std::move(name), ParseExpr());
    }
  }
  return ParseExpr();
}

std::unique_ptr<ast::If> Parser::ParseIf() {
  if (Peek()->kind != Token::Kind::KW_IF) {
    Throw("expected 'if'");
  }
  Loc begin = Bump()->begin;

  auto cond = ParseExpr();
  auto block = ParseBlock();
  if (Peek()->kind != Token::Kind::KW_ELSE) {
    return std::make_unique<ast::If>(begin, std::move(cond), std::move(block));
  }
  Bump();

  if (Peek()->kind == Token::Kind::KW_IF) {
    auto els = ParseIf();
    return std::make_unique<ast::If>(begin, std::move(cond), std::move(block),
                                     std::move(els));
  } else {
    auto els = ParseBlock();
    return std::make_unique<ast::If>(begin, std::move(cond), std::move(block),
                                     std::move(els));
  }
}

std::unique_ptr<ast::Block> Parser::ParseBlock() {
  if (Peek()->kind != Token::Kind::LBRACE) {
    Throw("expected %s", ToString(Token::Kind::LBRACE).c_str());
  }
  Loc begin = Bump()->begin;

  unique_deque<ast::Stmt> stmts;
  while (true) {
    if (Peek()->kind == Token::Kind::RBRACE) {
      break;
    }
    auto stmt = ParseStmt();
    stmts.push_back(std::move(stmt));
    if (Peek()->kind == Token::Kind::RBRACE) {
      break;
    }
    if (Peek()->kind == Token::Kind::SEMI) {
      Bump();
    }
  }
  Loc end = Bump()->begin;
  return std::make_unique<ast::Block>(begin, end, std::move(stmts));
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
      file->fn_decls.push_back(std::move(fn));
    } else {
      Throw("unknown top-level token");
    }
    needs_nl = true;
  }
  return std::move(file);
}

template <typename... Args>
void Parser::Throw(const std::string& fmt, Args... args) {
  throw LocError::Create(Peek()->begin, fmt, args...);
}

}  // namespace felis
