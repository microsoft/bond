add_bond_test (unit_test_comm
    unit_test_comm.cpp
    unit_test_comm.bond
    COMM)

target_use_cxx11 (unit_test_comm)

# TODO: needed for lock, should be abstracted away in platform specific stuff
target_compile_definitions (unit_test_comm PRIVATE -D_WIN32_WINNT=0x0600)

target_link_libraries (unit_test_comm PRIVATE
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
    ${Boost_CHRONO_LIBRARY}
    unit_test_main)
