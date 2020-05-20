#ifndef FELIS_STRING_STRING_H_
#define FELIS_STRING_STRING_H_

#include <cstdio>
#include <string>
#include <vector>

#include "check/decl.h"
#include "check/type.h"
#include "node/ast.h"
#include "node/hir.h"
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
/* std::string ToString(hir::Binary::Op op); */
std::string ToString(const Ty&);
std::string ToString(const Typed&);
std::string ToString(const FuncType&);
std::string ToString(const Untyped&);
std::string ToString(const ast::Stmt::Kind&);
std::string ToString(const Decl::Kind&);
std::string ToString(const Decl&);
/* std::string ToString(hir::Stmt::Kind); */

}  // namespace felis

#endif  // FELIS_STRING_STRING_H_
