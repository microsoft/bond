add_bond_test (event_example event.bond event.cpp COMM)

target_compile_definitions (event PRIVATE -D_WIN32_WINNT=0x0600)
