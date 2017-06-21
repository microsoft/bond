add_bond_codegen (dll.bond
    OPTIONS --header=\\\"dllexample_dynlink.h\\\" --apply=compact --export-attribute=DLLEXAMPLE_DYNLINK)

add_library (dll_example_lib EXCLUDE_FROM_ALL
    SHARED
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/dll_types.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/dll_apply.cpp)

target_include_directories(dll_example_lib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_compile_definitions (dll_example_lib PRIVATE -DBUILDING_DLLEXAMPLE_DLL)

add_target_to_folder (dll_example_lib)

target_link_libraries (dll_example_lib PUBLIC
    bond)

add_bond_test (dll_example
    using_dll.cpp)

target_compile_definitions (dll_example PRIVATE -DUSING_DLLEXAMPLE_DLL)

target_include_directories(dll_example PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries (dll_example PUBLIC
    dll_example_lib)
