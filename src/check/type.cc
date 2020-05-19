#include "type.h"

namespace felis {

static uint64_t id = 0;

std::shared_ptr<Type> MakeUnresolved() {
  return std::make_shared<UnresolvedType>(id++);
}

std::shared_ptr<Type> MakeUntypedInt() {
  return std::make_shared<Type>(Type::Kind::UNTYPED_INT);
}

std::shared_ptr<Type> MakeUntypedFloat() {
  return std::make_shared<Type>(Type::Kind::UNTYPED_FLOAT);
}

}  // namespace felis
