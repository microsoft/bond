add_bond_test (layer_v2 layer_v2.bond layer_v2.cpp COMM)

target_compile_definitions (layer_v2 PRIVATE -D_WIN32_WINNT=0x0600)
