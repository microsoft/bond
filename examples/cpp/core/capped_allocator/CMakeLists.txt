add_bond_codegen (capped_allocator.bond
    OPTIONS 
        --allocator=\"bond::capped_allocator<std::allocator<void> >\"
        --header=\"<bond/core/capped_allocator.h>\")

add_bond_test (capped_allocator
	${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/capped_allocator_types.cpp
	capped_allocator.cpp)
