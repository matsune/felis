
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
    case Token::Kind::LBRACK:
      return "[";
    case Token::Kind::RBRACK:
      return "]";
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
  }
  UNREACHABLE
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
      return "<";
    case hir::Binary::Op::LE:
      return "<=";
    case hir::Binary::Op::GT:
      return ">";
    case hir::Binary::Op::GE:
      return ">=";
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

std::string ToString(const std::shared_ptr<Type> &t) {
  if (!t) return "null";

  if (t->IsVoid()) return "void";
  if (t->IsI8()) return "i8";
  if (t->IsI16()) return "i16";
  if (t->IsI32()) return "i32";
  if (t->IsI64()) return "i64";
  if (t->IsF32()) return "f32";
  if (t->IsF64()) return "f64";
  if (t->IsBool()) return "bool";
  if (t->IsString()) return "string";

  if (t->IsFunc()) {
    auto func_type = dynamic_cast<FuncType *>(t.get());
    std::string str = "func(";
    for (auto it = func_type->args.begin(); it != func_type->args.end(); ++it) {
      bool is_begin = it == func_type->args.begin();
      if (!is_begin) str += ", ";
      str += ToString(*it);
    }
    str += ")";
    if (!func_type->ret->IsVoid()) {
      str += " -> " + ToString(func_type->ret);
    }
    return str;
  }
  if (t->IsUntyped()) {
    auto untyped = dynamic_cast<Untyped *>(t.get());
    std::stringstream s;
    if (t->IsUnresolved()) s << "unresolved (ref: " << untyped->Ref() << ")";
    if (t->IsUntypedInt()) s << "untyped int (ref: " << untyped->Ref() << ")";
    if (t->IsUntypedFloat())
      s << "untyped float (ref: " << untyped->Ref() << ")";
    return s.str();
  }
  if (t->IsArray()) {
    auto array = dynamic_cast<ArrayType *>(t.get());
    std::stringstream s;
    s << "[" << ToString(array->elem) << ", " << array->size << "]";
    return s.str();
  }
  UNREACHABLE
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

std::string ToString(const DeclKind &kind) {
  switch (kind) {
    case DeclKind::EXT:
      return "EXT";
    case DeclKind::FN:
      return "FN";
    case DeclKind::ARG:
      return "ARG";
    case DeclKind::VAR:
      return "VAR";
    case DeclKind::LET:
      return "LET";
  };
}

std::string ToString(const std::shared_ptr<Decl> &decl) {
  if (!decl) return "null";

  std::stringstream ss;
  ss << "Decl {name: " << decl->name << ", kind: " << ToString(decl->kind)
     << ", type: " << ToString(decl->type) << "} (" << decl.get() << ")";
  return ss.str();
}

}  // namespace felis
