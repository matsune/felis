#include "syntax/parser.h"

#include <cassert>
#include <map>
#include <vector>

#include "macro.h"
#include "unique.h"

namespace felis {

std::unique_ptr<ast::File> ParseAst(std::ifstream& in) {
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

ast::BinaryOp::Kind binop_from_tok(Token::Kind kind) {
  switch (kind) {
    case Token::Kind::PLUS:
      return ast::BinaryOp::Kind::ADD;
    case Token::Kind::MINUS:
      return ast::BinaryOp::Kind::SUB;
    case Token::Kind::STAR:
      return ast::BinaryOp::Kind::MUL;
    case Token::Kind::SLASH:
      return ast::BinaryOp::Kind::DIV;
    case Token::Kind::PERCENT:
      return ast::BinaryOp::Kind::MOD;
    case Token::Kind::GT:
      return ast::BinaryOp::Kind::GT;
    case Token::Kind::GE:
      return ast::BinaryOp::Kind::GE;
    case Token::Kind::LT:
      return ast::BinaryOp::Kind::LT;
    case Token::Kind::LE:
      return ast::BinaryOp::Kind::LE;
    case Token::Kind::EQEQ:
      return ast::BinaryOp::EQEQ;
    case Token::Kind::NEQ:
      return ast::BinaryOp::NEQ;
    default:
      UNREACHABLE
  }
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
      file->externs.push_back(Extern());
    } else if (Match(Token::Kind::KW_FN)) {
      file->funcs.push_back(Func());
    } else {
      Throw("unknown top-level token");
    }
    needs_nl = true;
  }
  return std::move(file);
}

// 'ext' <FnProto>
ast::Extern* Parser::Extern() {
  assert(Match(Token::Kind::KW_EXT));
  auto kw_ext = Bump();
  auto begin = kw_ext->begin;
  auto proto = FnProto();
  auto end = proto->end;
  return new ast::Extern(begin, end, proto);
}

// <FnProto> <Block>
ast::Func* Parser::Func() {
  auto proto = FnProto();
  auto begin = proto->begin;
  auto block = Block();
  auto end = block->end;
  return new ast::Func(begin, end, proto, block);
}

ast::FnProto* Parser::FnProto() {
  // 'fn'
  auto kw_fn = Must(Token::Kind::KW_FN);
  auto fn_begin = kw_fn->begin;
  // name
  auto name_tok = Must(Token::Kind::IDENT);
  auto name = new ast::Ident(name_tok->begin, name_tok->end, name_tok->val);
  // args
  auto args = FnArgs();
  // ret type
  if (Match(Token::Kind::ARROW)) {
    Bump();
    auto ret_type = TypeName();
    return new ast::FnProto(fn_begin, ret_type->end, name, args, ret_type);
  } else {
    return new ast::FnProto(fn_begin, args->end, name, args);
  }
}

// start with '('
ast::FnArgs* Parser::FnArgs() {
  auto begin = Must(Token::Kind::LPAREN)->begin;
  std::vector<ast::FnArg*> args;

  if (Match(Token::Kind::RPAREN)) {
    Loc end = Bump()->begin;
    return new ast::FnArgs(begin, end, args);
  }

  args.push_back(FnArg());

  while (Match(Token::Kind::COMMA)) {
    Bump();
    args.push_back(FnArg());
  }

  Loc end = Must(Token::Kind::RPAREN)->begin;
  return new ast::FnArgs(begin, end, args);
}

ast::FnArg* Parser::FnArg() {
  auto ty_or_name = TypeName();
  auto begin = ty_or_name->begin;
  if (Match(Token::Kind::COLON)) {
    // name: type
    if (node_isa<ast::ArrayType>(ty_or_name)) {
      throw LocError::Create(ty_or_name->begin, "arg name is type");
    }
    auto name = node_cast<ast::Ident>(ty_or_name);
    Bump();
    auto type = TypeName();
    auto end = type->end;
    return new ast::FnArg(begin, end, name, type);
  } else {
    // type only
    auto type = ty_or_name;
    auto end = type->end;
    return new ast::FnArg(begin, end, nullptr, type);
  }
}

// <type-name> ::= <ident>
//               | '[' <type-name> ',' <integer> ']'
ast::AstNode* Parser::TypeName() {
  if (Match(Token::Kind::LBRACK)) {
    // ArrayType
    // [T, size]
    auto begin = Bump()->begin;
    auto elem = TypeName();
    Must(Token::Kind::COMMA);
    auto size_tok = Must(Token::Kind::LIT_INT);
    auto size_lit =
        ast::Literal::Int(size_tok->begin, size_tok->end, size_tok->val);
    auto end = Must(Token::Kind::RBRACK)->end;
    return new ast::ArrayType(begin, end, elem, size_lit);
  } else {
    auto ident = Must(Token::Kind::IDENT);
    return new ast::Ident(ident->begin, ident->end, ident->val);
  }
};

// <call> ::= <ident> '(' <expr>* ')'
// <primary-expr> ::= <call>
//                  | <ident>
//                  | <constant>
//                  | '(' <expr> ')'
//                  | <array>
//                  | <block>
//                  | <if-stmt>
ast::AstNode* Parser::PrimaryExpr() {
  switch (Peek()->kind) {
    case Token::Kind::LBRACE:
      return Block();
    case Token::Kind::KW_IF:
      return If();
    case Token::Kind::LBRACK:
      return Array();
    case Token::Kind::IDENT: {
      auto tok = Bump();
      auto ident = new ast::Ident(tok->begin, tok->end, tok->val);
      if (Match(Token::Kind::LPAREN)) {
        // call
        auto end = Bump()->end;
        std::deque<ast::AstNode*> args;
        if (Match(Token::Kind::RPAREN)) {
          Bump();
          return new ast::Call(ident->begin, end, ident, args);
        }
        args.push_back(Expr());
        while (Match(Token::Kind::COMMA)) {
          Bump();
          args.push_back(Expr());
        }
        end = Must(Token::Kind::RPAREN)->begin;
        return new ast::Call(ident->begin, end, ident, args);
      }
      return ident;
    } break;
    case Token::Kind::LIT_INT: {
      auto tok = Bump();
      return ast::Literal::Int(tok->begin, tok->end, tok->val);
    } break;
    case Token::Kind::LIT_FLOAT: {
      auto tok = Bump();
      return ast::Literal::Float(tok->begin, tok->end, tok->val);
    } break;
    case Token::Kind::LIT_BOOL: {
      auto tok = Bump();
      return ast::Literal::Bool(tok->begin, tok->end, tok->val);
    } break;
    case Token::Kind::LIT_CHAR: {
      auto tok = Bump();
      return ast::Literal::Char(tok->begin, tok->end, tok->r);
    } break;
    case Token::Kind::LIT_STR: {
      auto tok = Bump();
      return ast::Literal::String(tok->begin, tok->end, tok->val);
    } break;
    case Token::Kind::LPAREN: {
      Bump();
      auto expr = Expr();
      Must(Token::Kind::RPAREN);
      return expr;
    } break;
    default:
      Throw("unknown primary token");
  }
}

// <postfix-expr> ::= <primary-expr>
//                  | <postfix-expr> '[' <expr> ']'
ast::AstNode* Parser::PostfixExpr() {
  ast::AstNode* expr = PrimaryExpr();
  while (!Peek()->nl && Match(Token::Kind::LBRACK)) {
    Bump();
    auto idx_expr = Expr();
    auto end = Must(Token::Kind::RBRACK)->end;
    expr = new ast::Index(expr->begin, end, expr, idx_expr);
  }
  return expr;
}

// <unary-expr> ::= <postfix-expr>
//                | <unary-op> <expr>
ast::AstNode* Parser::UnaryExpr() {
  if (Match(Token::Kind::MINUS) || Match(Token::Kind::NOT)) {
    auto tok = Bump();
    auto op_kind =
        tok->kind == Token::Kind::MINUS ? ast::UnaryOp::NEG : ast::UnaryOp::NOT;
    auto unary_op = new ast::UnaryOp(tok->begin, tok->end, op_kind);
    auto expr = Expr();
    return new ast::Unary(unary_op->begin, expr->end, unary_op, expr);
  }
  return PostfixExpr();
}

// <mul-expr> ::= <unary-expr>
//              | <mul-expr> '*' <unary-expr>
//              | <mul-expr> '/' <unary-expr>
//              | <mul-expr> '%' <unary-expr>
ast::AstNode* Parser::MulExpr() {
  ast::AstNode* lhs = UnaryExpr();
  while (Match(Token::Kind::STAR) || Match(Token::Kind::SLASH) ||
         Match(Token::Kind::PERCENT)) {
    auto op_tok = Bump();
    auto op_kind = binop_from_tok(op_tok->kind);
    auto op = new ast::BinaryOp(op_tok->begin, op_tok->end, op_kind);
    auto rhs = UnaryExpr();
    lhs = new ast::Binary(lhs->begin, rhs->end, lhs, rhs, op);
  }
  return lhs;
}

// <add-expr> ::= <mul-expr>
//              | <add-expr> '+' <mul-expr>
//              | <add-expr> '-' <mul-expr>
ast::AstNode* Parser::AddExpr() {
  ast::AstNode* lhs = MulExpr();
  while (Match(Token::Kind::PLUS) || Match(Token::Kind::MINUS)) {
    auto op_tok = Bump();
    auto op_kind = binop_from_tok(op_tok->kind);
    auto op = new ast::BinaryOp(op_tok->begin, op_tok->end, op_kind);
    auto rhs = MulExpr();
    lhs = new ast::Binary(lhs->begin, rhs->end, lhs, rhs, op);
  }
  return lhs;
}

// <rel-expr> ::= <add-expr>
//              | <rel-expr> '<' <add-expr>
//              | <rel-expr> '<=' <add-expr>
//              | <rel-expr> '>' <add-expr>
//              | <rel-expr> '>=' <add-expr>
ast::AstNode* Parser::RelExpr() {
  ast::AstNode* lhs = AddExpr();
  while (Match(Token::Kind::LT) || Match(Token::Kind::LE) ||
         Match(Token::Kind::GT) || Match(Token::Kind::GE)) {
    auto op_tok = Bump();
    auto op_kind = binop_from_tok(op_tok->kind);
    auto op = new ast::BinaryOp(op_tok->begin, op_tok->end, op_kind);
    auto rhs = AddExpr();
    lhs = new ast::Binary(lhs->begin, rhs->end, lhs, rhs, op);
  }
  return lhs;
}

// <eq-expr> ::= <rel-expr>
//             | <eq-expr> '==' <rel-expr>
//             | <eq-expr> '!=' <rel-expr>
ast::AstNode* Parser::EqExpr() {
  ast::AstNode* lhs = RelExpr();
  while (Match(Token::Kind::EQEQ) || Match(Token::Kind::NEQ)) {
    auto op_tok = Bump();
    auto op_kind = binop_from_tok(op_tok->kind);
    auto op = new ast::BinaryOp(op_tok->begin, op_tok->end, op_kind);
    auto rhs = RelExpr();
    lhs = new ast::Binary(lhs->begin, rhs->end, lhs, rhs, op);
  }
  return lhs;
}

ast::AstNode* Parser::Expr() { return EqExpr(); }

// <ret-stmt> ::= 'ret' <expr>?
ast::RetStmt* Parser::RetStmt() {
  auto kw_ret = Bump();
  if (Peek()->nl) {
    return new ast::RetStmt(kw_ret->begin, kw_ret->end);
  }
  auto expr = Expr();
  return new ast::RetStmt(kw_ret->begin, expr->end, expr);
}

// <spec> ::= 'var'
//          | 'let'
// <var-decl-stmt> ::= <spec> <ident> (':' <type-name>)? '=' <expr>
ast::VarDeclStmt* Parser::VarDeclStmt() {
  auto spec = Bump();
  auto is_let = spec->kind == Token::Kind::KW_LET;
  auto ident_tok = Must(Token::Kind::IDENT);
  auto ident = new ast::Ident(ident_tok->begin, ident_tok->end, ident_tok->val);
  ast::AstNode* type = nullptr;
  if (Match(Token::Kind::COLON)) {
    Bump();
    type = TypeName();
  }
  Must(Token::Kind::EQ);
  auto expr = Expr();
  return new ast::VarDeclStmt(spec->begin, expr->end, is_let, ident, type,
                              expr);
}

// <stmt> ::= <ret-stmt>
//          | <var-decl-stmt>
//          | <assign-stmt>
//          | <expr>
ast::AstNode* Parser::Stmt() {
  if (Match(Token::Kind::KW_RET)) {
    return RetStmt();
  } else if (Match(Token::Kind::KW_LET) || Match(Token::Kind::KW_VAR)) {
    return VarDeclStmt();
  } else {
    auto left = Expr();
    if (Match(Token::Kind::EQ)) {
      Bump();
      auto expr = Expr();
      return new ast::AssignStmt(left->begin, expr->end, left, expr);
    }
    return left;
  }
}

// <if-stmt> ::= 'if' <expr> <block> <else-stmt>?
ast::If* Parser::If() {
  auto begin = Must(Token::Kind::KW_IF)->begin;
  auto cond = Expr();
  auto block = Block();
  if (!Match(Token::Kind::KW_ELSE)) {
    return new ast::If(begin, block->end, cond, block);
  }
  Bump();

  ast::AstNode* els;
  if (Match(Token::Kind::KW_IF)) {
    els = If();
  } else {
    els = Block();
  }
  return new ast::If(begin, els->end, cond, block, els);
}

// <block> ::= '{' <stmt>* '}'
ast::Block* Parser::Block() {
  auto begin = Must(Token::Kind::LBRACE)->begin;
  std::deque<ast::AstNode*> stmts;
  while (!Match(Token::Kind::RBRACE)) {
    auto stmt = Stmt();
    stmts.push_back(stmt);
    if (Match(Token::Kind::SEMI)) {
      Bump();
    }
  }
  Loc end = Bump()->begin;
  return new ast::Block(begin, end, stmts);
}

// <array> ::= '[' <expr-list>? ']'
//
// <expr-list> ::= <expr>
//               | <expr-list> ',' <expr>
ast::Array* Parser::Array() {
  auto begin = Must(Token::Kind::LBRACK)->begin;
  std::deque<ast::AstNode*> exprs;
  if (Match(Token::Kind::RBRACK)) {
    return new ast::Array(begin, Bump()->end, exprs);
  }

  while (true) {
    exprs.push_back(Expr());
    if (Match(Token::Kind::COMMA)) {
      Bump();
    } else {
      break;
    }
  }
  auto end = Must(Token::Kind::RBRACK)->end;
  return new ast::Array(begin, end, exprs);
}

template <typename... Args>
void Parser::Throw(const std::string& fmt, Args... args) {
  throw LocError::Create(Peek()->begin, fmt, args...);
}

}  // namespace felis
