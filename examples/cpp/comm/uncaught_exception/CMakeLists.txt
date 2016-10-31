add_bond_test (uncaught_exception uncaught_exception.bond uncaught_exception.cpp COMM)

target_use_cxx11 (uncaught_exception)
target_compile_definitions (uncaught_exception PRIVATE -D_WIN32_WINNT=0x0600)
