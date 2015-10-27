function (add_unit_test_library name)
    list (INSERT ARGV 1 EXCLUDE_FROM_ALL)
    add_library (${ARGV})
    add_target_to_folder (${name})
    add_dependencies (${name}
        bond
        unit_test_codegen1
        unit_test_codegen2
        unit_test_codegen3)
    target_include_directories (${name} PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
    target_compile_definitions (${name} PUBLIC
 #       -DENABLE_TEST_CASE=0x20010000
        -DBOND_COMPACT_BINARY_PROTOCOL
        -DBOND_SIMPLE_BINARY_PROTOCOL
        -DBOND_FAST_BINARY_PROTOCOL
        -DBOND_SIMPLE_JSON_PROTOCOL
        -DBOOST_ENABLE_ASSERT_HANDLER)
    target_use_cxx11 (${name})
    target_link_libraries (${name} INTERFACE unit_test_idl bond)
    target_link_libraries (unit_test PRIVATE ${name})
endfunction()

add_bond_codegen (TARGET unit_test_codegen2
    unit_test_core.bond
    apply_test.bond
    import_test1.bond
    import_test2.bond
    scope_test1.bond
    scope_test2.bond
    validation.bond
    cmdargs.bond
    OPTIONS
        --header=\\\"custom_protocols.h\\\")

add_bond_codegen (TARGET unit_test_codegen1
    unit_test.bond
    OPTIONS
        --using=\"static_string=std::array<char, {0}>\"
        --using=\"static_wstring=std::array<wchar_t, {0}>\"
        --using=\"simple_list=SimpleList<{0}>\"
        --header=\\\"custom_protocols.h\\\"
        --header=\\\"container_extensibility.h\\\")

add_bond_codegen (TARGET unit_test_codegen3
    allocator_test.bond
    OPTIONS
        --using=\"Vector=std::vector<{0}, detail::TestAllocator<{0}> >\"
        --header=\\\"custom_protocols.h\\\"
        --header=\\\"allocators.h\\\"
        --allocator=TestAllocator)

add_bond_test (unit_test init.cpp)

target_use_cxx11 (unit_test)

add_library (unit_test_main EXCLUDE_FROM_ALL main.cpp)
add_target_to_folder (unit_test_main)

target_link_libraries (unit_test PRIVATE
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
    unit_test_main)

# Full unit test is too big, .pdb files exceed compiler limit;
# Since debugging such large executable is not practical anyways,
# we disable .pdb generation.
# TODO: add support for selecting individual test case via ENABLE_TEST_CASE macro.
no_pdb()

# split across multiple libraries to parallelize build
add_unit_test_library (unit_test_idl
    idl.cpp)

add_unit_test_library (unit_test_lib1
    skip_id_tests.cpp
    skip_type_tests.cpp
    inheritance.cpp
    protocol_test.cpp
    bonded_tests.cpp
    blob_tests.cpp
    basic_tests.cpp
    validate_tests.cpp
    required_fields_tests.cpp
    custom_protocols.cpp
    cmdargs.cpp
    metadata_tests.cpp)

add_unit_test_library (unit_test_lib2
    json_tests.cpp
    pass_through.cpp
    associative_container_extensibility.cpp
    apply_tests.cpp
    apply_init.cpp
    may_omit_fields.cpp)

add_unit_test_library (unit_test_lib3
    numeric_convertions.cpp)

add_unit_test_library (unit_test_lib4
    generics_test.cpp
    merge_test.cpp)

add_unit_test_library (unit_test_lib5
    list_tests.cpp
    serialization_test.cpp
    allocator_test.cpp
    set_tests.cpp
    basic_type_lists.cpp
    nullable.cpp)

add_unit_test_library (unit_test_lib6
    marshal.cpp
    container_extensibility.cpp
    basic_type_map.cpp
    enum_convertions.cpp
    exception_tests.cpp
    maybe_tests.cpp)
