#include "check/decl.h"

#include <iostream>

#include "string/string.h"

namespace felis {

void Decl::Debug() {
  std::cout << "Decl " << name << " kind: " << ToString(kind)
            << " type: " << ToString(type.get()) << std::endl;
}

}  // namespace felis
