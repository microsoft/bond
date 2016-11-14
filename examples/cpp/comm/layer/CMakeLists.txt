add_bond_test (layer layer.bond layer.cpp COMM)

target_compile_definitions (layer PRIVATE -D_WIN32_WINNT=0x0600)
