cmake_minimum_required(VERSION 3.5.1)
project(felis-test)

include_directories(lib/googletest/googletest/include)
add_subdirectory(lib/googletest/googletest)

add_executable(felis-test test.cpp)
target_link_libraries(felis-test felis gtest_main)
