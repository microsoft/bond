add_bond_codegen (comm_dll.bond
    OPTIONS --header=\\\"dllexample_dynlink.h\\\" --apply=compact --apply-attribute=DLLEXAMPLE_DYNLINK
    COMM)

add_library (comm_dll_example_lib EXCLUDE_FROM_ALL
    SHARED
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/comm_dll_types.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/comm_dll_apply.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/comm_dll_comm.cpp)

target_include_directories(comm_dll_example_lib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_compile_definitions (comm_dll_example_lib PRIVATE -DBUILDING_DLLEXAMPLE_DLL)

add_target_to_folder (comm_dll_example_lib)

target_link_libraries (comm_dll_example_lib PUBLIC
    bond)

add_bond_test (comm_dll_example
    using_comm_dll.cpp)

target_compile_definitions (comm_dll_example PRIVATE -DUSING_DLLEXAMPLE_DLL)

target_include_directories(comm_dll_example PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries (comm_dll_example PUBLIC
    comm_dll_example_lib)
