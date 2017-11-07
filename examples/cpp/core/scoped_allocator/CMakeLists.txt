add_bond_codegen (scoped_allocator.bond
    OPTIONS 
        --allocator=\"examples::scoped_allocator::MyAllocator<char>\"
        --header=\\\"allocator.h\\\"
        --alloc-ctors
        --scoped-alloc)

add_bond_test (scoped_allocator
	${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/scoped_allocator_types.cpp
	scoped_allocator.cpp)
