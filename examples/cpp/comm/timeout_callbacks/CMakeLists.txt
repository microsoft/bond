add_bond_test (timeout_callbacks timeout_callbacks.bond timeout_callbacks.cpp COMM)

target_compile_definitions (timeout_callbacks PRIVATE -D_WIN32_WINNT=0x0600)
