add_bond_test (error error.bond error.cpp COMM)

target_use_cxx11 (error)
target_compile_definitions (error PRIVATE -D_WIN32_WINNT=0x0600)
