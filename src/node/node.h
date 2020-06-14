#ifndef FELIS_NODE_NODE_H_
#define FELIS_NODE_NODE_H_

#include "loc.h"

namespace felis {

struct Node {
  virtual ~Node(){};
};

template <typename E>
E *node_cast_ornull(Node *t) {
  return dynamic_cast<E *>(t);
}

template <typename E>
const E *node_cast_ornull(const Node *t) {
  return dynamic_cast<const E *>(t);
}

template <typename E>
bool node_isa(Node *t) {
  auto e = dynamic_cast<E *>(t);
  return e != nullptr;
}

template <typename E>
bool node_isa(const Node *t) {
  auto e = dynamic_cast<const E *>(t);
  return e != nullptr;
}

template <typename E>
E *node_cast(Node *t) {
  assert(node_isa<E>(t));
  return dynamic_cast<E *>(t);
}

template <typename E>
const E *node_cast(const Node *t) {
  assert(node_isa<const E>(t));
  return dynamic_cast<const E *>(t);
}

}  // namespace felis

#endif  // FELIS_NODE_NODE_H_
