add_bond_codegen (string_ref.bond
    OPTIONS 
        --using=\"string_ref=boost::string_ref\"
        --header=\"<string_ref.h>\"
        --type-aliases)

add_bond_test (string_ref
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/string_ref_types.cpp
    string_ref.cpp)
