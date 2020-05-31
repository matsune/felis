#ifndef FELIS_STRING_STRING_H_
#define FELIS_STRING_STRING_H_

#include <cstdio>
#include <string>
#include <vector>

#include "check/decl.h"
#include "check/type.h"
#include "node/ast.h"
#include "node/mir.h"
#include "syntax/token.h"

namespace felis {

template <typename... Args>
std::string format(const std::string& fmt, Args... args) {
  size_t len = std::snprintf(nullptr, 0, fmt.c_str(), args...);
  std::vector<char> buf(len + 1);
  std::snprintf(&buf[0], len + 1, fmt.c_str(), args...);
  return std::string(&buf[0], &buf[0] + len);
}

std::string ToString(const Token::Kind&);
std::string ToString(const ast::BinaryOp::Op&);
std::string ToString(const std::shared_ptr<Type>&);
std::string ToString(const ast::Stmt::Kind&);
std::string ToString(const ast::Expr::Kind&);
std::string ToString(const DeclKind&);
std::string ToString(const std::shared_ptr<Decl>&);

// MIR
std::string ToString(const std::shared_ptr<mir::RValue>&);
std::string ToString(const std::shared_ptr<mir::LValue>&);
std::string ToString(const mir::BinaryInst::Op&);
std::string ToString(const mir::CmpInst::Op&);
std::string ToString(const mir::UnaryInst::Op&);

}  // namespace felis

#endif  // FELIS_STRING_STRING_H_
