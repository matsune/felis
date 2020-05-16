#ifndef FELIS_MACRO_H_
#define FELIS_MACRO_H_

namespace felis {

#define UNREACHABLE                                                        \
  std::cerr << "UNREACHABLE " << __FILE__ << ":" << __LINE__ << std::endl; \
  exit(2);

#define UNIMPLEMENTED                                                        \
  std::cerr << "UNIMPLEMENTED " << __FILE__ << ":" << __LINE__ << std::endl; \
  exit(2);

}  // namespace felis

#endif  // FELIS_MACRO_H_
