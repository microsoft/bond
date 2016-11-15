add_bond_test (unit_test_comm
    unit_test_comm.cpp
    unit_test_comm.bond
    COMM)

target_use_cxx11 (unit_test_comm)

# TODO: needed for lock, should be abstracted away in platform specific stuff
target_compile_definitions (unit_test_comm PRIVATE -D_WIN32_WINNT=0x0600)

# Build common code into its own library.
add_library (comm_test_common EXCLUDE_FROM_ALL "main.cpp")
add_target_to_folder (comm_test_common)
target_include_directories (comm_test_common PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
target_link_libraries (comm_test_common PRIVATE
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

target_link_libraries (unit_test_comm PRIVATE
    comm_test_common
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
    ${Boost_CHRONO_LIBRARY})
