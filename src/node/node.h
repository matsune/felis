#ifndef FELIS_NODE_NODE_H_
#define FELIS_NODE_NODE_H_

#include "loc.h"

namespace felis {

struct Node {
  virtual Loc Begin() const = 0;
  virtual Loc End() const = 0;
};

}  // namespace felis

#endif  // FELIS_NODE_NODE_H_
