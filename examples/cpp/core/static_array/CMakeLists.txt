add_bond_codegen (static_array.bond
    OPTIONS 
        --using=\"array=std::array<{0}, {1}>\"
        --header=\"<static_array.h>\")

add_bond_test (static_array
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/static_array_types.cpp
    static_array.cpp)
