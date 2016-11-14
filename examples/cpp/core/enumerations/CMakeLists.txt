add_bond_codegen (enumerations.bond
    ENUM_HEADER) 

add_bond_test (enumerations
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/enumerations_types.cpp
    enumerations.cpp)

