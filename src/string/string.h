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

std::string ToString(Token::Kind kind);
std::string ToString(ast::BinaryOp::Op op);
std::string ToString(hir::Binary::Op op);
std::string ToString(Decl::Kind);
std::string ToString(Type*);

}  // namespace felis

#endif  // FELIS_STRING_STRING_H_
