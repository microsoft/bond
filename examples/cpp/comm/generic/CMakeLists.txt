add_bond_test (generic_example generic.bond generic.cpp COMM)

target_compile_definitions (generic_example PRIVATE -D_WIN32_WINNT=0x0600)
