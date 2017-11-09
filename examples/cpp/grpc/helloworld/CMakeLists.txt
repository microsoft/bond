add_bond_test (grpc-helloworld helloworld.bond helloworld.cpp GRPC)

cxx_target_compile_definitions (MSVC grpc-helloworld PRIVATE -D_WIN32_WINNT=0x0600)

target_link_libraries(grpc-helloworld PRIVATE grpc++)
