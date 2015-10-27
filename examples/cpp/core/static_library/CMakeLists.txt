add_bond_codegen (lib/static_library.bond
    OPTIONS --apply=compact)

add_library (static_library_lib EXCLUDE_FROM_ALL
    STATIC
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/static_library_types.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/static_library_apply.cpp)

add_target_to_folder (static_library_lib)

target_link_libraries (static_library_lib PUBLIC
    bond)

add_bond_test (static_library
    exe/static_library.cpp)

target_link_libraries (static_library PRIVATE
    static_library_lib)
