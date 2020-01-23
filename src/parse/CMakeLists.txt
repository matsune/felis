FIND_PACKAGE(FLEX)
SET(FLEX_OUT ${CMAKE_CURRENT_SOURCE_DIR}/lexer.cpp)
SET(FLEX_SRC ${CMAKE_CURRENT_SOURCE_DIR}/lexer.ll)

IF(FLEX_FOUND)
    ADD_CUSTOM_COMMAND(
      OUTPUT ${FLEX_OUT}
      DEPENDS ${FLEX_SRC}
      COMMAND ${FLEX_EXECUTABLE}
              --outfile=${FLEX_OUT}
              ${FLEX_SRC}
      COMMENT "Generating ${FLEX_OUT}"
    )
ENDIF()

file(GLOB parse_SRCS *.cpp *.hpp)
add_library(parse STATIC ${parse_SRCS} ${FLEX_OUT})
