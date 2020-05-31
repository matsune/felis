#include "syntax/parser.h"

#include <cassert>
#include <map>
#include <vector>

#include "unique.h"

namespace felis {

std::unique_ptr<felis::ast::File> ParseAst(std::ifstream& in) {
  unique_deque<felis::Token> tokens;
  Lexer lexer(in);
  bool is_end(false);
  while (!is_end) {
    auto token = lexer.Next();
    is_end = token->kind == felis::Token::Kind::END;
    tokens.push_back(std::move(token));
  }
  return felis::Parser(std::move(tokens)).Parse();
}

namespace {

inline bool is_lit(Token::Kind kind) {
  return kind == Token::Kind::LIT_INT || kind == Token::Kind::LIT_FLOAT ||
         kind == Token::Kind::LIT_STR || kind == Token::Kind::LIT_CHAR ||
         kind == Token::Kind::LIT_BOOL;
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
      UNREACHABLE
  }
}

inline const std::unique_ptr<Token>& Parser::Peek() const {
  return tokens_.front();
}

inline const std::unique_ptr<Token>& Parser::Peek2() const {
  return tokens_[1];
}

inline std::unique_ptr<Token> Parser::Bump() { return tokens_.move_front(); }

inline bool Parser::Match(Token::Kind kind) { return Peek()->kind == kind; }

std::unique_ptr<ast::Extern> Parser::ParseExtern() {
  assert(Match(Token::Kind::KW_EXT));
  auto ext = Bump();
  auto begin = ext->begin;
  auto proto = ParseFnProto();
  return std::make_unique<ast::Extern>(begin, std::move(proto));
}

std::unique_ptr<ast::FnDecl> Parser::ParseFnDecl() {
  auto proto = ParseFnProto();
  auto block = ParseBlock(nullptr);
  if (!proto->ret) {
    // void function block
    block->as_stmt = true;
  }
  return std::make_unique<ast::FnDecl>(std::move(proto), std::move(block));
}

std::unique_ptr<ast::FnProto> Parser::ParseFnProto() {
  if (!Match(Token::Kind::KW_FN)) {
    Throw("expected 'fn'");
  }
  auto fn = Bump();
  Loc fn_begin = fn->begin;

  if (!Match(Token::Kind::IDENT)) {
    Throw("expected ident");
  }
  auto name_tok = Bump();
  auto name =
      std::make_unique<ast::Ident>(nullptr, name_tok->begin, name_tok->val);

  auto fn_args = ParseFnArgs();

  if (Match(Token::Kind::ARROW)) {
    Bump();
    auto ret_ty = ParseType();
    return std::make_unique<ast::FnProto>(
        fn_begin, std::move(name), std::move(fn_args), std::move(ret_ty));
  } else {
    return std::make_unique<ast::FnProto>(fn_begin, std::move(name),
                                          std::move(fn_args));
  }
}

// start with '('
std::unique_ptr<ast::FnArgs> Parser::ParseFnArgs() {
  if (!Match(Token::Kind::LPAREN)) {
    Throw("expected (");
  }
  Loc begin = Bump()->begin;

  std::vector<std::unique_ptr<ast::FnArg>> args;
  if (Match(Token::Kind::RPAREN)) {
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

  while (Match(Token::Kind::COMMA)) {
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

  if (!Match(Token::Kind::RPAREN)) {
    Throw("expected )");
  }
  Loc end = Bump()->begin;

  return std::make_unique<ast::FnArgs>(begin, end, std::move(args));
}

std::unique_ptr<ast::FnArg> Parser::ParseFnArg() {
  if (!Match(Token::Kind::IDENT)) {
    Throw("expected ident");
  }
  auto name_or_ty_tok = Bump();

  if (Match(Token::Kind::COLON)) {
    /// name: type
    Bump();

    auto type_name = ParseType();

    auto name = std::make_unique<ast::Ident>(nullptr, name_or_ty_tok->begin,
                                             name_or_ty_tok->val);
    return std::make_unique<ast::FnArg>(std::move(type_name), std::move(name));
  } else {
    // only type
    return std::make_unique<ast::FnArg>(std::make_unique<ast::TypeIdent>(
        name_or_ty_tok->begin, name_or_ty_tok->val));
  }
}

std::unique_ptr<ast::Type> Parser::ParseType() {
  if (Match(Token::Kind::LBRACK)) {
    auto begin = Bump()->begin;
    auto elem = ParseType();
    if (!Match(Token::Kind::COMMA)) {
      Throw("expected ,");
    }
    Bump();

    if (!Match(Token::Kind::LIT_INT)) {
      Throw("expected int literal");
    }
    auto size_tok = Bump();
    auto size = std::make_unique<ast::Lit>(nullptr, size_tok->begin,
                                           ast::Lit::Kind::INT, size_tok->val);

    if (!Match(Token::Kind::RBRACK)) {
      Throw("expected ]");
    }
    auto end = Bump()->end;
    return std::make_unique<ast::ArrayType>(begin, end, std::move(elem),
                                            std::move(size));
  } else {
    if (!Match(Token::Kind::IDENT)) {
      Throw("expected ident");
    }
    auto ident = Bump();
    return std::make_unique<ast::TypeIdent>(ident->begin, ident->val);
  }
};

std::unique_ptr<ast::Expr> Parser::ParseExpr(ast::Block* parent, uint8_t prec) {
  std::unique_ptr<ast::UnaryOp> un_op;
  if (Match(Token::Kind::MINUS)) {
    Loc begin = Bump()->begin;
    un_op = std::make_unique<ast::UnaryOp>(begin, ast::UnaryOp::Op::NEG);
  } else if (Match(Token::Kind::NOT)) {
    Loc begin = Bump()->begin;
    un_op = std::make_unique<ast::UnaryOp>(begin, ast::UnaryOp::Op::NOT);
  }

  auto lhs = ParsePrimary(parent);

  if (Match(Token::Kind::LPAREN)) {
    if (lhs->ExprKind() != ast::Expr::Kind::IDENT) {
      Throw("cannot call non function");
    }
    if (!Match(Token::Kind::LPAREN)) {
      Throw("expected %s", ToString(Token::Kind::LPAREN).c_str());
    }
    Bump();

    unique_deque<ast::Expr> args;
    Loc end;
    if (Match(Token::Kind::RPAREN)) {
      end = Bump()->begin;
    } else {
      args.push_back(ParseExpr(parent));

      if (Match(Token::Kind::RPAREN)) {
        end = Bump()->end;
        return std::make_unique<ast::CallExpr>(
            parent, end, unique_cast<ast::Ident>(std::move(lhs)),
            std::move(args));
      }

      while (!Match(Token::Kind::RPAREN)) {
        if (!Match(Token::Kind::COMMA)) {
          Throw("expected %s", ToString(Token::Kind::COMMA).c_str());
        }
        Bump();

        args.push_back(ParseExpr(parent));
      }
      end = Bump()->begin;
    }
    lhs = std::make_unique<ast::CallExpr>(
        parent, end, unique_cast<ast::Ident>(std::move(lhs)), std::move(args));
  }
  if (un_op) {
    lhs = std::make_unique<ast::UnaryExpr>(parent, std::move(un_op),
                                           std::move(lhs));
  }

  if (Peek()->nl) {
    return lhs;
  }

  while (true) {
    if (!is_bin_op(Peek()->kind)) {
      return lhs;
    }
    auto& peek = Peek();
    ast::BinaryOp::Op op = binop_from_tok(peek->kind);
    Loc begin = peek->begin;
    auto bin_op = std::make_unique<ast::BinaryOp>(begin, op);
    if (op <= prec) {
      return lhs;
    }

    Bump();
    lhs = std::make_unique<ast::BinaryExpr>(
        parent, std::move(lhs), std::move(bin_op), ParseExpr(parent, op));
  }
}

std::unique_ptr<ast::Expr> Parser::ParsePrimary(ast::Block* parent) {
  auto& token = Peek();
  Loc begin = token->begin;
  switch (token->kind) {
    case Token::Kind::LBRACE:
      return ParseBlock(parent);
    case Token::Kind::KW_IF:
      return ParseIf(parent);
    case Token::Kind::IDENT:
      return std::make_unique<ast::Ident>(parent, begin, Bump()->val);
    case Token::Kind::LIT_INT: {
      return std::make_unique<ast::Lit>(parent, begin, ast::Lit::Kind::INT,
                                        Bump()->val);
    } break;
    case Token::Kind::LIT_FLOAT: {
      return std::make_unique<ast::Lit>(parent, begin, ast::Lit::Kind::FLOAT,
                                        Bump()->val);
    } break;
    case Token::Kind::LIT_BOOL: {
      return std::make_unique<ast::Lit>(parent, begin, ast::Lit::Kind::BOOL,
                                        Bump()->val);
    } break;
    case Token::Kind::LIT_CHAR: {
      return std::make_unique<ast::Lit>(parent, begin, ast::Lit::Kind::CHAR,
                                        Bump()->val);
    } break;
    case Token::Kind::LIT_STR: {
      return std::make_unique<ast::Lit>(parent, begin, ast::Lit::Kind::STRING,
                                        Bump()->val);
    } break;
    case Token::Kind::LPAREN: {
      Bump();
      auto expr = ParseExpr(parent);
      if (!Match(Token::Kind::RPAREN)) {
        Throw("expected )");
      }
      Bump();
      return expr;
    } break;
    case Token::Kind::LBRACK: {
      auto begin = Bump()->begin;
      unique_deque<ast::Expr> exprs;
      while (true) {
        exprs.push_back(ParseExpr(parent));
        if (Match(Token::Kind::COMMA)) {
          Bump();
        } else {
          break;
        }
      }
      if (!Match(Token::Kind::RBRACK)) {
        Throw("expected ]");
      }
      auto end = Bump()->end;
      return std::make_unique<ast::ArrayExpr>(parent, begin, end,
                                              std::move(exprs));
    } break;
    default:
      Throw("unknown primary token");
  }
  UNREACHABLE
}

std::unique_ptr<ast::Stmt> Parser::ParseStmt(ast::Block* parent) {
  if (Match(Token::Kind::KW_RET)) {
    // Ret
    auto& ret = Peek();
    Loc ret_begin = ret->begin;
    Loc ret_end = ret->end;
    Bump();

    if (Match(Token::Kind::SEMI)) {
      Bump();
      return std::make_unique<ast::RetStmt>(parent, ret_begin, ret_end);
    }
    if (Peek()->nl) {
      return std::make_unique<ast::RetStmt>(parent, ret_begin, ret_end);
    }
    auto expr = ParseExpr(parent);
    ret_end = expr->End();
    return std::make_unique<ast::RetStmt>(parent, ret_begin, ret_end,
                                          std::move(expr));

  } else if (Peek()->kind == Token::Kind::KW_LET ||
             Peek()->kind == Token::Kind::KW_VAR) {
    // variable decl
    auto kw = Bump();
    bool is_let = kw->kind == Token::Kind::KW_LET;
    Loc kw_begin = kw->begin;

    if (!Match(Token::Kind::IDENT)) {
      Throw("expected %s", ToString(Token::Kind::IDENT).c_str());
    }
    auto ident = Bump();
    auto name = std::make_unique<ast::Ident>(parent, ident->begin, ident->val);

    std::unique_ptr<ast::Type> type_name = nullptr;
    if (Match(Token::Kind::COLON)) {
      // let a: i32 = ...
      Bump();
      type_name = ParseType();
    }
    if (!Match(Token::Kind::EQ)) {
      Throw("expected %s", ToString(Token::Kind::EQ).c_str());
    }
    Bump();

    return std::make_unique<ast::VarDeclStmt>(
        parent, kw_begin, is_let, std::move(name), std::move(type_name),
        ParseExpr(parent));
  } else if (Match(Token::Kind::IDENT)) {
    if (Peek2()->kind == Token::Kind::EQ) {
      // assign stmt
      auto bump = Bump();
      auto name = std::make_unique<ast::Ident>(parent, bump->begin, bump->val);
      Bump();
      return std::make_unique<ast::AssignStmt>(parent, std::move(name),
                                               ParseExpr(parent));
    }
  }
  auto expr = ParseExpr(parent);
  expr->as_stmt = true;
  return std::move(expr);
}

std::unique_ptr<ast::If> Parser::ParseIf(ast::Block* parent) {
  if (!Match(Token::Kind::KW_IF)) {
    Throw("expected 'if'");
  }
  Loc begin = Bump()->begin;

  auto cond = ParseExpr(parent);
  auto block = ParseBlock(parent);
  if (!Match(Token::Kind::KW_ELSE)) {
    return std::make_unique<ast::If>(parent, begin, std::move(cond),
                                     std::move(block));
  }
  Bump();

  if (Match(Token::Kind::KW_IF)) {
    auto els = ParseIf(parent);
    return std::make_unique<ast::If>(parent, begin, std::move(cond),
                                     std::move(block), std::move(els));
  } else {
    auto els = ParseBlock(parent);
    return std::make_unique<ast::If>(parent, begin, std::move(cond),
                                     std::move(block), std::move(els));
  }
}

std::unique_ptr<ast::Block> Parser::ParseBlock(ast::Block* parent) {
  if (!Match(Token::Kind::LBRACE)) {
    Throw("expected %s", ToString(Token::Kind::LBRACE).c_str());
  }
  Loc begin = Bump()->begin;

  auto block = std::make_unique<ast::Block>(parent, begin);
  while (true) {
    if (Match(Token::Kind::RBRACE)) {
      break;
    }
    auto stmt = ParseStmt(block.get());
    block->stmts.push_back(std::move(stmt));
    if (Match(Token::Kind::RBRACE)) {
      break;
    }
    if (Match(Token::Kind::SEMI)) {
      Bump();
    }
  }
  Loc end = Bump()->begin;
  block->end = end;
  return std::move(block);
}

std::unique_ptr<ast::File> Parser::Parse() {
  auto file = std::make_unique<ast::File>();

  bool needs_nl = false;
  while (!Match(Token::Kind::END)) {
    if (needs_nl) {
      if (Match(Token::Kind::SEMI)) {
        Bump();
      } else if (Peek()->nl) {
        // nothing
      } else {
        Throw("needs \\n or ;");
      }
    }
    if (Match(Token::Kind::KW_EXT)) {
      file->externs.push_back(ParseExtern());
    } else if (Match(Token::Kind::KW_FN)) {
      file->fn_decls.push_back(ParseFnDecl());
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
