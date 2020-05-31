
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
    default:
      UNREACHABLE
  }
}

std::string ToString(const ast::BinaryOp::Op &op) {
  switch (op) {
    case ast::BinaryOp::Op::EQEQ:
      return "==";
    case ast::BinaryOp::Op::NEQ:
      return "!=";
    case ast::BinaryOp::Op::LT:
      return "<";
    case ast::BinaryOp::Op::LE:
      return "<=";
    case ast::BinaryOp::Op::GT:
      return ">";
    case ast::BinaryOp::Op::GE:
      return ">=";
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

std::string ToString(const ast::Expr::Kind &kind) {
  switch (kind) {
    case ast::Expr::Kind::IDENT:
      return "IDENT";
    case ast::Expr::Kind::BINARY:
      return "BINARY";
    case ast::Expr::Kind::LIT:
      return "LIT";
    case ast::Expr::Kind::CALL:
      return "CALL";
    case ast::Expr::Kind::UNARY:
      return "UNARY";
    case ast::Expr::Kind::BLOCK:
      return "BLOCK";
    case ast::Expr::Kind::IF:
      return "IF";
    case ast::Expr::Kind::ARRAY:
      return "ARRAY";
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

// MIR
std::string ToString(const std::shared_ptr<mir::RValue> &value) {
  std::stringstream s;
  switch (value->RValueKind()) {
    case mir::RValue::Kind::CONST_INT: {
      auto c = (const std::shared_ptr<mir::ConstantInt> &)value;
      s << "constant " << ToString(c->type) << " " << c->val;
    } break;
    case mir::RValue::Kind::CONST_FLOAT: {
      auto c = (const std::shared_ptr<mir::ConstantFloat> &)value;
      s << "constant " << ToString(c->type) << " " << c->val;
    } break;
    case mir::RValue::Kind::CONST_BOOL: {
      auto c = (const std::shared_ptr<mir::ConstantBool> &)value;
      s << "constant bool " << (c->val ? "true" : "false");
    } break;
    case mir::RValue::Kind::CONST_STRING: {
      auto c = (const std::shared_ptr<mir::ConstantString> &)value;
      s << "constant string " << c->val;
    } break;
    case mir::RValue::Kind::VAL: {
      auto c = (const std::shared_ptr<mir::Val> &)value;
      s << "$" << c->id << ": " << ToString(c->type);
    } break;
  }
  return s.str();
}

std::string ToString(const std::shared_ptr<mir::LValue> &value) {
  std::stringstream s;
  s << "$" << value->id << ": *" << ToString(value->type);
  return s.str();
};

std::string ToString(const mir::BinaryInst::Op &op) {
  switch (op) {
    case mir::BinaryInst::Op::ADD:
      return "Add";
    case mir::BinaryInst::Op::SUB:
      return "Sub";
    case mir::BinaryInst::Op::MUL:
      return "Mul";
    case mir::BinaryInst::Op::DIV:
      return "Div";
    case mir::BinaryInst::Op::MOD:
      return "Mod";
  }
}

std::string ToString(const mir::CmpInst::Op &op) {
  switch (op) {
    case mir::CmpInst::Op::EQEQ:
      return "Eq";
    case mir::CmpInst::Op::NEQ:
      return "NotEq";
    case mir::CmpInst::Op::LT:
      return "LT";
    case mir::CmpInst::Op::LE:
      return "LE";
    case mir::CmpInst::Op::GT:
      return "GT";
    case mir::CmpInst::Op::GE:
      return "GE";
  }
}

std::string ToString(const mir::UnaryInst::Op &op) {
  switch (op) {
    case mir::UnaryInst::Op::NEG:
      return "Neg";
    case mir::UnaryInst::Op::NOT:
      return "Not";
  }
}

}  // namespace felis
