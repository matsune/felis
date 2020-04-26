#ifndef FELIS_IR_TYPE_H_
#define FELIS_IR_TYPE_H_

#include <memory>
#include <string>

#include "syntax/ast.h"

namespace felis {

enum Ty { UNKNOWN, VOID, INT, CHAR, STRING, FLOAT, BOOL };

}  // namespace felis

#endif  // FELIS_IR_TYPE_H_
