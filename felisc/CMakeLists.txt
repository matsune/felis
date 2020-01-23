add_executable(felisc main.cpp)
target_link_libraries(felisc common parse ${llvm_libs})
