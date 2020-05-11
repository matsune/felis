#ifndef FELIS_MACRO_H_
#define FELIS_MACRO_H_

namespace felis {

#define UNREACHABLE                                                        \
  std::cerr << "unreachable " << __FILE__ << ":" << __LINE__ << std::endl; \
  exit(2);

}  // namespace felis

#endif  // FELIS_MACRO_H_
