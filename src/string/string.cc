
#include "string.h"

namespace felis {

std::string ToString(Token::Kind kind) {
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
    case Type::Kind::I64:
      return "i64";
    case Type::Kind::F32:
      return "f32";
    case Type::Kind::F64:
      return "f64";
    case Type::Kind::BOOL:
      return "bool";
    case Type::Kind::CHAR:
      return "char";
    case Type::Kind::STRING:
      return "string";
    default:
      std::cout << "unreachable" << std::endl;
      std::terminate();
  }
}

}  // namespace felis
