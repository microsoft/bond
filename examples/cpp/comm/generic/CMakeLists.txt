add_bond_test (generic generic.bond generic.cpp COMM)

target_compile_definitions (generic PRIVATE -D_WIN32_WINNT=0x0600)
