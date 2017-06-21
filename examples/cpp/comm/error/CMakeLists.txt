add_bond_test (error_example error.bond error.cpp COMM)

target_compile_definitions (error_example PRIVATE -D_WIN32_WINNT=0x0600)
