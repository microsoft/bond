add_bond_codegen (multiprecision.bond
    OPTIONS 
        --using=int512=boost::multiprecision::int512_t
        --using=int1024=boost::multiprecision::int1024_t
        --using=integer=boost::multiprecision::cpp_int
        --header=\"<multiprecision.h>\")

add_bond_test (multiprecision
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/multiprecision_types.cpp
    multiprecision.cpp)
