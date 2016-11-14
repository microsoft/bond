add_bond_test (transport exampletransport.bond exampletransport.cpp COMM)

target_compile_definitions (transport PRIVATE -D_WIN32_WINNT=0x0600)
