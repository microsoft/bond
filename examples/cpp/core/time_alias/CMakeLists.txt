add_bond_codegen (time_alias.bond
    OPTIONS 
        --using=\"time=boost::posix_time::ptime\"
        --header=\"<time_alias.h>\"
        --type-aliases)

add_bond_test (time_alias
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/time_alias_types.cpp
    time_alias.cpp)

target_link_libraries (time_alias PRIVATE
    ${Boost_DATE_TIME_LIBRARY})
