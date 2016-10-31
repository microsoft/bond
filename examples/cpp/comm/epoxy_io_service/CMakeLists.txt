add_bond_test (epoxy_io_service epoxy_io_service.bond epoxy_io_service.cpp COMM)

target_use_cxx11 (epoxy_io_service)
target_compile_definitions (epoxy_io_service PRIVATE -D_WIN32_WINNT=0x0600)
