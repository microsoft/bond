add_bond_test (conn_state_changed_v2 conn_state_changed_v2.bond conn_state_changed_v2.cpp COMM)

target_compile_definitions (conn_state_changed_v2 PRIVATE -D_WIN32_WINNT=0x0600)
