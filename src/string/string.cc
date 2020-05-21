
#include "string.h"

#include <sstream>

namespace felis {

std::string ToString(const Token::Kind &kind) {
  switch (kind) {
    case Token::Kind::END:
      return "END";
    case Token::Kind::IDENT:
      return "IDENT";
    case Token::Kind::LIT_INT:
      return "LIT_INT";
    case Token::Kind::LIT_FLOAT:
      return "LIT_FLOAT";
    case Token::Kind::LIT_BOOL:
      return "LIT_BOOL";
    case Token::Kind::LIT_CHAR:
      return "CHAR";
    case Token::Kind::LIT_STR:
      return "LIT_STR";
    case Token::Kind::PLUS:
      return "+";
    case Token::Kind::MINUS:
      return "-";
    case Token::Kind::STAR:
      return "*";
    case Token::Kind::SLASH:
      return "/";
    case Token::Kind::PERCENT:
      return "%";
    case Token::Kind::AND:
      return "&";
    case Token::Kind::OR:
      return "|";
    case Token::Kind::CARET:
      return "^";
    case Token::Kind::SHL:
      return "<<";
    case Token::Kind::SHR:
      return ">>";
    case Token::Kind::ANDAND:
      return "&&";
    case Token::Kind::OROR:
      return "||";
    case Token::Kind::LT:
      return "<";
    case Token::Kind::LE:
      return "<=";
    case Token::Kind::GT:
      return ">";
    case Token::Kind::GE:
      return ">=";
    case Token::Kind::EQEQ:
      return "==";
    case Token::Kind::NEQ:
      return "!=";
    case Token::Kind::NOT:
      return "!";
    case Token::Kind::LPAREN:
      return "(";
    case Token::Kind::RPAREN:
      return ")";
    case Token::Kind::LBRACE:
      return "{";
    case Token::Kind::RBRACE:
      return "}";
    case Token::Kind::EQ:
      return "=";
    case Token::Kind::SEMI:
      return ";";
    case Token::Kind::COLON:
      return ":";
    case Token::Kind::COMMA:
      return ",";
    case Token::Kind::ARROW:
      return "->";
    default:
      return "unknown";
  }
}

std::string ToString(const ast::BinaryOp::Op &op) {
  switch (op) {
    case ast::BinaryOp::Op::EQEQ:
      return "==";
    case ast::BinaryOp::Op::NEQ:
      return "!=";
    case ast::BinaryOp::Op::LT:
      return ">";
    case ast::BinaryOp::Op::LE:
      return ">=";
    case ast::BinaryOp::Op::GT:
      return "<";
    case ast::BinaryOp::Op::GE:
      return "<=";
    case ast::BinaryOp::Op::ADD:
      return "+";
    case ast::BinaryOp::Op::SUB:
      return "-";
    case ast::BinaryOp::Op::MUL:
      return "*";
    case ast::BinaryOp::Op::DIV:
      return "/";
    case ast::BinaryOp::Op::MOD:
      return "%";
  }
}

std::string ToString(const hir::Binary::Op &op) {
  switch (op) {
    case hir::Binary::Op::EQEQ:
      return "==";
    case hir::Binary::Op::NEQ:
      return "!=";
    case hir::Binary::Op::LT:
      return ">";
    case hir::Binary::Op::LE:
      return ">=";
    case hir::Binary::Op::GT:
      return "<";
    case hir::Binary::Op::GE:
      return "<=";
    case hir::Binary::Op::ADD:
      return "+";
    case hir::Binary::Op::SUB:
      return "-";
    case hir::Binary::Op::MUL:
      return "*";
    case hir::Binary::Op::DIV:
      return "/";
    case hir::Binary::Op::MOD:
      return "%";
  }
}

std::string ToString(const Decl::Kind &kind) {
  switch (kind) {
    case Decl::Kind::EXT:
      return "EXT";
    case Decl::Kind::FN:
      return "FN";
    case Decl::Kind::ARG:
      return "ARG";
    case Decl::Kind::VAR:
      return "VAR";
    case Decl::Kind::LET:
      return "LET";
  };
}

std::string ToString(const Ty &t) {
  if (t.IsTyped()) {
    return ToString(dynamic_cast<const Typed &>(t));
  } else {
    return ToString(dynamic_cast<const Untyped &>(t));
  }
}

std::string ToString(const Typed &t) {
  switch (t.TypedKind()) {
    case Typed::Kind::VOID:
      return "void";
    case Typed::Kind::I32:
      return "i32";
    case Typed::Kind::I64:
      return "i64";
    case Typed::Kind::F32:
      return "f32";
    case Typed::Kind::F64:
      return "f64";
    case Typed::Kind::BOOL:
      return "bool";
    case Typed::Kind::STRING:
      return "string";
    case Typed::Kind::FUNC:
      return ToString(dynamic_cast<const FuncType &>(t));
  }
}

std::string ToString(const FuncType &t) {
  std::string str = "func(";
  int i = 0;
  for (auto &arg : t.args) {
    if (i++ > 0) {
      str += ", ";
    }
    str += ToString(*arg);
  }
  str += ")";
  if (!t.ret->IsVoid()) {
    str += " -> " + ToString(*t.ret);
  }
  return str;
}

std::string ToString(const Untyped &t) {
  std::stringstream ss;
  switch (t.UntypedKind()) {
    case Untyped::Kind::INT:
      ss << "UntypedInt ref: ";
      break;
    case Untyped::Kind::FLOAT:
      ss << "UntypedFloat ref: ";
      break;
  }
  ss << t.GetRef();
  return ss.str();
}

std::string ToString(const ast::Stmt::Kind &kind) {
  switch (kind) {
    case ast::Stmt::Kind::ASSIGN:
      return "ASSIGN";
    case ast::Stmt::Kind::EXPR:
      return "EXPR";
    case ast::Stmt::Kind::RET:
      return "RET";
    case ast::Stmt::Kind::VAR_DECL:
      return "VAR_DECL";
  }
}

std::string ToString(const Decl &decl) {
  std::stringstream ss;
  ss << "Decl {name: " << decl.name << ", kind: " << ToString(decl.kind)
     << ", type: " << ToString(*decl.type) << "}";
  return ss.str();
}

}  // namespace felis
