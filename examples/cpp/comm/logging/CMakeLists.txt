add_bond_test (logging logging.bond logging.cpp COMM)

target_compile_definitions (logging PRIVATE -D_WIN32_WINNT=0x0600)
