
#include "string.h"

#include <sstream>

#include "macro.h"

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
    default:
      UNREACHABLE
  }
}

std::string ToString(const ast::BinaryOp::Kind &op) {
  switch (op) {
    case ast::BinaryOp::Kind::EQEQ:
      return "==";
    case ast::BinaryOp::Kind::NEQ:
      return "!=";
    case ast::BinaryOp::Kind::LT:
      return "<";
    case ast::BinaryOp::Kind::LE:
      return "<=";
    case ast::BinaryOp::Kind::GT:
      return ">";
    case ast::BinaryOp::Kind::GE:
      return ">=";
    case ast::BinaryOp::Kind::ADD:
      return "+";
    case ast::BinaryOp::Kind::SUB:
      return "-";
    case ast::BinaryOp::Kind::MUL:
      return "*";
    case ast::BinaryOp::Kind::DIV:
      return "/";
    case ast::BinaryOp::Kind::MOD:
      return "%";
  }
}

std::string ToString(const Type::Kind &kind) {
  switch (kind) {
    case Type::Kind::UNRESOLVED:
      return "UNRESOLVED";
    case Type::Kind::UNTYPED_INT:
      return "UNTYPED_INT";
    case Type::Kind::UNTYPED_FLOAT:
      return "UNTYPED_FLOAT";
    case Type::Kind::VOID:
      return "void";
    case Type::Kind::I8:
      return "i8";
    case Type::Kind::I16:
      return "i16";
    case Type::Kind::I32:
      return "i32";
    case Type::Kind::I64:
      return "i64";
    case Type::Kind::F32:
      return "f32";
    case Type::Kind::F64:
      return "f64";
    case Type::Kind::BOOL:
      return "bool";
    case Type::Kind::STRING:
      return "string";
    case Type::Kind::FUNC:
      return "func";
    case Type::Kind::ARRAY:
      return "array";
    case Type::Kind::PTR:
      return "ptr";
  };
}

// std::string ToString(const std::shared_ptr<Type> ty) {
//  if (!ty) return "null";
//  ToString(*ty);
//}

// std::string ToString(std::shared_ptr<Type> &ty) {
//  return ToString(const_cast<const std::shared_ptr<Type> &>(ty));
//}

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

std::string ToString(const Type ty) {
  std::stringstream ss;
  if (ty.IsFunc()) {
    ss << "func (";
    auto args = ty.GetArgs();
    for (auto it = args.begin(); it != args.end(); ++it) {
      if (it != args.begin()) {
        ss << ", ";
      }
      ss << ToString(**it);
    }
    ss << ") -> " << (ty.GetRet() ? ToString(*ty.GetRet()) : "void");
  } else if (ty.IsArray()) {
    ss << "[" << ToString(*ty.GetElem()) << ", " << ty.GetSize() << "]";
  } else if (ty.IsPtr()) {
    ss << ToString(*ty.GetElem()) << "*";
  } else if (ty.IsUnresolved() || ty.IsUntypedInt() || ty.IsUntypedFloat()) {
    ss << ToString(ty.GetKind());
  } else {
    ss << ToString(ty.GetKind());
  }
  return ss.str();
}

std::string ToString(const std::shared_ptr<Decl> &decl) {
  if (!decl) return "null";

  std::stringstream ss;
  ss << "Decl {name: " << decl->name << ", kind: " << ToString(decl->kind)
     << ", type: " << ToString(*decl->type) << "} (" << decl.get() << ")";
  return ss.str();
}

}  // namespace felis
