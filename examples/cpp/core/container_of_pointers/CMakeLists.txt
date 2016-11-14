add_bond_codegen (container_of_pointers.bond
    OPTIONS 
        --using=\"PtrList=std::list<std::shared_ptr<{0}> >\" 
        --header=\"<memory>\")

add_bond_test (container_of_pointers
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/container_of_pointers_types.cpp
    container_of_pointers.cpp)

