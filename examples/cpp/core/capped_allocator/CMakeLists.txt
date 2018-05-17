add_bond_codegen (capped_allocator.bond
    OPTIONS 
        --allocator=\"bond::ext::capped_allocator<>\"
        --header=\"<bond/ext/capped_allocator.h>\")

add_bond_test (capped_allocator
	${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/capped_allocator_types.cpp
	capped_allocator.cpp)

# MSVC has bugs in some containers (e.g. std::string) where constructors
# accepting an allocator are marked as `noexcept` while they do allocate
# data when checked iterators are enabled (Debug builds by default). This
# terminates the application when `bond::ext::capped_allocator` throws.
if (MSVC)
    add_definitions (-D_ITERATOR_DEBUG_LEVEL=0)
endif()
