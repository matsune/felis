file(GLOB SRCS **/*.cpp **/*.hpp)
add_library(felis STATIC ${SRCS})
