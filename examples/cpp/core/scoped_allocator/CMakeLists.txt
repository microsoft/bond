add_bond_codegen (scoped_alloc.bond
    OPTIONS 
        --allocator=\"examples::scoped_allocator::MyAllocator<char>\"
        --header=\\\"allocator.h\\\"
        --alloc-ctors
        --scoped-alloc)

add_bond_test (scoped_alloc
        ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/scoped_alloc_types.cpp
        scoped_alloc.cpp)
