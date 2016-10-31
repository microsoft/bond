add_bond_test (passthrough passthrough.bond passthrough.cpp COMM)

target_use_cxx11 (passthrough)
target_compile_definitions (passthrough PRIVATE -D_WIN32_WINNT=0x0600)
