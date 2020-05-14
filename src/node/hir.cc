#include "node/hir.h"

namespace felis {

namespace hir {

bool Expr::IsConstant() {
  if (ExprKind() == Kind::VALUE) {
    auto value = (Value*)this;
    return value->IsConstant();
  } else {
    return false;
  }
}

void Expr::Debug() {
  /* std::cout << "EXP " << pos.line << ":" << pos.column << ": "; */
  switch (ExprKind()) {
    case Expr::Kind::BINARY:
      std::cout << "BINARY" << std::endl;
      break;
    case Expr::Kind::VALUE: {
      auto value = (Value*)this;
      switch (value->ValueKind()) {
        case Value::Kind::CONSTANT: {
          auto constant = (Constant*)this;
          switch (constant->ConstantKind()) {
            case Constant::Kind::BOOL:
              std::cout << "BOOL CONSTANT" << std::endl;
              break;
            case Constant::Kind::INT:
              std::cout << "INT CONSTANT" << std::endl;
              break;
            case Constant::Kind::FLOAT:
              std::cout << "FLOAT CONSTANT" << std::endl;
              break;
            case Constant::Kind::CHAR:
              std::cout << "CHAR CONSTANT" << std::endl;
              break;
            case Constant::Kind::STRING:
              std::cout << "STRING CONSTANT" << std::endl;
              break;
          }
        } break;
        case Value::Kind::VARIABLE:
          std::cout << "VARIABLE" << std::endl;
          break;
      }
    } break;
    case Expr::Kind::CALL:
      std::cout << "CALL" << std::endl;
      break;
    case Expr::Kind::UNARY:
      std::cout << "UNARY" << std::endl;
      break;
  };
}

}  // namespace hir

}  // namespace felis
