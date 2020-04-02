cmake_minimum_required(VERSION 3.5.1)
enable_language(CXX)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

project(felis)

# embedding LLVM
if(NOT CMAKE_EXPORT_COMPILE_COMMANDS)
  find_package(LLVM REQUIRED CONFIG)
  if(NOT LLVM_FOUND )
    message(FATAL_ERROR "LLVM package can't be found. Set CMAKE_PREFIX_PATH variable to LLVM's installation prefix.")
  endif()
  message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
  message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
  include_directories(${LLVM_INCLUDE_DIRS})
  add_definitions(${LLVM_DEFINITIONS})
  llvm_map_components_to_libnames(llvm_libs core native)
else()
  include_directories(${CMAKE_SOURCE_DIR}/include/llvm-9)
  include_directories(${CMAKE_SOURCE_DIR}/include/llvm-c-9)
endif()


include_directories(src)

add_subdirectory(src)
add_subdirectory(felisc)
add_subdirectory(test)
