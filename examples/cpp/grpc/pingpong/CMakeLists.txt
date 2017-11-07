add_bond_test (grpc-pingpong pingpong.bond pingpong.cpp GRPC)

cxx_target_compile_definitions (MSVC grpc-pingpong PRIVATE -D_WIN32_WINNT=0x0600)

target_link_libraries(grpc-pingpong PRIVATE grpc++)
