if (MSVC)
    add_bond_test (unit_test_comm
        unit_test_comm.cpp
        unit_test_comm.bond
        unit_test_comm_async.cpp
        unit_test_comm_basic.cpp
        unit_test_comm_core.cpp
        unit_test_comm_error.cpp
        unit_test_comm_event.bond
        unit_test_comm_event.cpp
        unit_test_comm_forward.cpp
        unit_test_comm_generic.bond
        unit_test_comm_generic_type.cpp
        unit_test_comm_generic_service.cpp
        unit_test_comm_jumbo.cpp
        unit_test_comm_layers.cpp
        unit_test_comm_protocol.cpp
        unit_test_comm_session.bond
        unit_test_comm_session.cpp
        unit_test_comm_timeout.cpp
        unit_test_comm_unexpected.cpp
        unit_test_comm_utils.bond
        unit_test_comm_utils.cpp
        unit_test_comm_void.bond
        unit_test_comm_void.cpp
        COMM)
else()
    add_bond_test (unit_test_comm
        unit_test_comm.cpp
        unit_test_comm.bond
        unit_test_comm_async.cpp
        unit_test_comm_basic.cpp
        unit_test_comm_core.cpp
        unit_test_comm_error.cpp
        unit_test_comm_event.bond
        unit_test_comm_event.cpp
        unit_test_comm_forward.cpp
        unit_test_comm_generic.bond
        unit_test_comm_generic_type.cpp
        unit_test_comm_generic_service.cpp
        unit_test_comm_jumbo.cpp
        unit_test_comm_layers.cpp
        unit_test_comm_protocol.cpp
        unit_test_comm_session.bond
        unit_test_comm_session.cpp
        unit_test_comm_timeout.cpp
        unit_test_comm_unexpected.cpp
        unit_test_comm_utils.bond
        unit_test_comm_utils.cpp
        unit_test_comm_void.bond
        unit_test_comm_void.cpp
        COMM
        BUILD_ONLY)
endif()

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

cxx_target_compile_options (Clang unit_test_comm PRIVATE -DBOOST_ASIO_HAS_STD_CHRONO)
cxx_target_compile_options (AppleClang unit_test_comm PRIVATE -DBOOST_ASIO_HAS_STD_CHRONO)

target_link_libraries (unit_test_comm PRIVATE
    comm_test_common
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
    ${Boost_CHRONO_LIBRARY})
