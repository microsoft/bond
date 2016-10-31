add_bond_test (pingpong pingpong.bond pingpong.cpp COMM)

target_use_cxx11 (pingpong)
target_compile_definitions (pingpong PRIVATE -D_WIN32_WINNT=0x0600)
