
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

std::string ToString(const Ty::Kind &kind) {
  switch (kind) {
    case Ty::Kind::UNRESOLVED:
      return "UNRESOLVED";
    case Ty::Kind::UNTYPED_INT:
      return "UNTYPED_INT";
    case Ty::Kind::UNTYPED_FLOAT:
      return "UNTYPED_FLOAT";
    case Ty::Kind::VOID:
      return "VOID";
    case Ty::Kind::I8:
      return "I8";
    case Ty::Kind::I16:
      return "I16";
    case Ty::Kind::I32:
      return "I32";
    case Ty::Kind::I64:
      return "I64";
    case Ty::Kind::F32:
      return "F32";
    case Ty::Kind::F64:
      return "F64";
    case Ty::Kind::BOOL:
      return "BOOL";
    case Ty::Kind::STRING:
      return "STRING";
    case Ty::Kind::FUNC:
      return "FUNC";
    case Ty::Kind::ARRAY:
      return "ARRAY";
    case Ty::Kind::PTR:
      return "PTR";
  };
}

std::string ToString(const std::shared_ptr<Ty> &ty) {
  std::stringstream ss;
  if (auto func_ty = std::dynamic_pointer_cast<FuncTy>(ty)) {
    ss << "func (";
    for (auto it = func_ty->args.begin(); it != func_ty->args.end(); ++it) {
      if (it != func_ty->args.begin()) {
        ss << ", ";
      }
      ss << ToString(*it);
    }
    ss << ") -> " << ToString(func_ty->ret);
  } else if (auto array_ty = std::dynamic_pointer_cast<ArrayTy>(ty)) {
    ss << "[" << ToString(array_ty->elem) << ", " << array_ty->size << "]";
  } else if (auto ptr_ty = std::dynamic_pointer_cast<PtrTy>(ty)) {
    ss << ToString(ptr_ty->ref) << "*";
  } else {
    ss << ToString(ty->kind);
  }
  return ss.str();
}

std::string ToString(std::shared_ptr<Ty> &ty) {
  return ToString(const_cast<const std::shared_ptr<Ty> &>(ty));
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
  s << " " << value.get();
  return s.str();
}

std::string ToString(const std::shared_ptr<mir::LValue> &value) {
  std::stringstream s;
  s << "$" << value->id << ": " << ToString(value->type);
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
