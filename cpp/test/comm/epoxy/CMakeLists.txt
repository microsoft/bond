add_bond_test (epoxy_protocol_tests
    epoxy_protocol_tests.h
    epoxy_protocol_tests.cpp
    epoxy_protocol_tests.bond
    COMM)

target_use_cxx11 (epoxy_protocol_tests)

# TODO: needed for lock, should be abstracted away in platform specific stuff
target_compile_definitions (epoxy_protocol_tests PRIVATE -D_WIN32_WINNT=0x0600)

target_link_libraries (epoxy_protocol_tests PRIVATE
    comm_test_common
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})
