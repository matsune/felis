file(GLOB SRCS **/*.cc **/*.h)
add_library(felis STATIC ${SRCS})
