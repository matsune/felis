#ifndef FELIS_PTR_H_
#define FELIS_PTR_H_

#include <memory>

namespace felis {

template <typename T, typename E>
std::unique_ptr<E> unique_cast(std::unique_ptr<T> ptr) {
  return std::unique_ptr<E>((E*)ptr.release());
}

}  // namespace felis

#endif  // FELIS_PTR_H_
