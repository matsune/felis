#ifndef FELIS_NODE_NODE_H_
#define FELIS_NODE_NODE_H_

#include "loc.h"

namespace felis {

struct Node {
  virtual ~Node(){};
};

template <typename E, typename T = Node>
bool node_isa(T *t) {
  auto e = dynamic_cast<E *>(t);
  return e != nullptr;
}

template <typename E, typename T = Node>
E *node_cast(T *t) {
  assert(node_isa<E>(t));
  return dynamic_cast<E *>(t);
}

template <typename E, typename T = Node>
E *node_cast_ornull(T *t) {
  return dynamic_cast<E *>(t);
}

}  // namespace felis

#endif  // FELIS_NODE_NODE_H_
