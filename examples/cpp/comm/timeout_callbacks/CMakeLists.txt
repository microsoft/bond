add_bond_test (timeout_callbacks timeout_callbacks.bond timeout_callbacks.cpp COMM)

target_use_cxx11 (timeout_callbacks)
target_compile_definitions (timeout_callbacks PRIVATE -D_WIN32_WINNT=0x0600)
