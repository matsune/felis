
#include "string.h"

namespace felis {

std::string ToString(TokenKind kind) {
  switch (kind) {
    case TokenKind::END:
      return "END";
    case TokenKind::IDENT:
      return "IDENT";
    case TokenKind::LIT_INT:
      return "LIT_INT";
    case TokenKind::LIT_FLOAT:
      return "LIT_FLOAT";
    case TokenKind::LIT_BOOL:
      return "LIT_BOOL";
    case TokenKind::LIT_CHAR:
      return "CHAR";
    case TokenKind::LIT_STR:
      return "LIT_STR";
    case TokenKind::PLUS:
      return "+";
    case TokenKind::MINUS:
      return "-";
    case TokenKind::STAR:
      return "*";
    case TokenKind::SLASH:
      return "/";
    case TokenKind::PERCENT:
      return "%";
    case TokenKind::AND:
      return "&";
    case TokenKind::OR:
      return "|";
    case TokenKind::CARET:
      return "^";
    case TokenKind::SHL:
      return "<<";
    case TokenKind::SHR:
      return ">>";
    case TokenKind::ANDAND:
      return "&&";
    case TokenKind::OROR:
      return "||";
    case TokenKind::LT:
      return "<";
    case TokenKind::LE:
      return "<=";
    case TokenKind::GT:
      return ">";
    case TokenKind::GE:
      return ">=";
    case TokenKind::EQEQ:
      return "==";
    case TokenKind::NEQ:
      return "!=";
    case TokenKind::NOT:
      return "!";
    case TokenKind::LPAREN:
      return "(";
    case TokenKind::RPAREN:
      return ")";
    case TokenKind::LBRACE:
      return "{";
    case TokenKind::RBRACE:
      return "}";
    case TokenKind::EQ:
      return "=";
    case TokenKind::SEMI:
      return ";";
    case TokenKind::COLON:
      return ":";
    case TokenKind::COMMA:
      return ",";
    case TokenKind::ARROW:
      return "->";
    default:
      return "unknown";
  }
}

/* std::string ToString(Ty ty) { */
/*   switch (ty) { */
/*     case Ty::UNKNOWN: */
/*       return "unknown"; */
/*     case Ty::VOID: */
/*       return "void"; */
/*     case Ty::BOOL: */
/*       return "bool"; */
/*     case Ty::INT: */
/*       return "int"; */
/*     case Ty::CHAR: */
/*       return "char"; */
/*     case Ty::FLOAT: */
/*       return "float"; */
/*     case Ty::STRING: */
/*       return "string"; */
/*   } */
/* } */

std::string ToString(ast::BinOp op) {
  switch (op) {
    case ast::BinOp::LT:
      return ">";
    case ast::BinOp::LE:
      return ">=";
    case ast::BinOp::GT:
      return "<";
    case ast::BinOp::GE:
      return "<=";
    case ast::BinOp::ADD:
      return "+";
    case ast::BinOp::SUB:
      return "-";
    case ast::BinOp::MUL:
      return "*";
    case ast::BinOp::DIV:
      return "/";
    case ast::BinOp::MOD:
      return "%";
  }
}

std::string ToString(Decl::Kind kind) {
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

std::string ToString(Type *type) {
  switch (type->TypeKind()) {
    case Type::Kind::FUNC: {
      auto funcType = (FuncType *)type;
      std::string str = "func(";
      int i = 0;
      for (auto &arg : funcType->args) {
        if (i++ > 0) {
          str += ", ";
        }
        str += ToString(arg.get());
      }
      str += ")";
      if (!funcType->ret->IsVoid()) {
        str += " -> " + ToString(funcType->ret.get());
      }
      return str;
    } break;
    case Type::Kind::VOID:
      return "void";
    case Type::Kind::I32:
      return "i32";
    case Type::Kind::F32:
      return "f32";
    case Type::Kind::BOOL:
      return "bool";
    case Type::Kind::CHAR:
      return "char";
    case Type::Kind::STRING:
      return "string";
  }
}

}  // namespace felis
