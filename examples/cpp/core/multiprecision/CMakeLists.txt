add_bond_codegen (multiprecision.bond
    OPTIONS
        --using=uint512=boost::multiprecision::uint512_t
        --using=uint1024=boost::multiprecision::uint1024_t
        --using=integer=boost::multiprecision::cpp_int
        --header=\"<multiprecision.h>\"
        --type-aliases)

add_bond_test (multiprecision
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/multiprecision_types.cpp
    multiprecision.cpp)

# Deep in the guts of Boost we get a warning about not using a "secure"
# routine. Suppress them for this example.
cxx_target_compile_options(MSVC multiprecision PRIVATE -D_SCL_SECURE_NO_WARNINGS)
