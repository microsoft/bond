# To add a new unit test suite, call add_unit_test at the bottom of this file on
# all of the files you want built into your test suite. The name of the suite
# will be derived from the name of the file that is given first.
function (add_unit_test)
    get_filename_component(name ${ARGV0} NAME_WE)
    set (files ${ARGV})

    add_bond_test (${name} EXCLUDE_FROM_ALL ${files})
    add_target_to_folder (${name})
    add_dependencies (${name}
        bond
        unit_test_codegen1
        unit_test_codegen2
        unit_test_codegen3
        unit_test_codegen4)
    target_include_directories (${name} PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
    target_compile_definitions (${name} PUBLIC
 #       -DENABLE_TEST_CASE=0x20010000
        -DBOND_COMPACT_BINARY_PROTOCOL
        -DBOND_SIMPLE_BINARY_PROTOCOL
        -DBOND_FAST_BINARY_PROTOCOL
        -DBOND_SIMPLE_JSON_PROTOCOL)
    target_link_libraries (${name} PRIVATE
      core_test_common)
endfunction()


# Codegen for .proto files.
add_protobuf_codegen_cpp (protobuf_writer)
add_library (protobuf_test_codegen
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/protobuf_writer.pb.cc")
target_include_directories (protobuf_test_codegen PUBLIC
    "${CMAKE_SOURCE_DIR}/thirdparty/protobuf/src"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}")
target_link_libraries(protobuf_test_codegen libprotobuf)
if (MSVC)
    # C4100: unreferenced formal parameter
    # C4125: decimal digit terminates octal escape sequence
    # C4456: declaration of 'size' hides previous local declaration
    # C4505: unreferenced local function has been removed
    # C4510: default constructor could not be generated
    # C4610: struct 'google::protobuf::Map...::Node' can never be instantiated - user defined constructor required
    # C4800: forcing value to bool 'true' or 'false' (performance warning)
    target_compile_options(protobuf_test_codegen PUBLIC /wd4100 /wd4125 /wd4456 /wd4505 /wd4510 /wd4610 /wd4800)
endif()
add_target_to_folder (protobuf_test_codegen)


# Build common code into its own library.
add_library (core_test_common
    EXCLUDE_FROM_ALL
    "main.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/unit_test_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/unit_test_core_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/allocator_test_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/import_test1_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/import_test1_apply.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/import_test2_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/apply_test_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/apply_test_apply.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/validation_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/scope_test1_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/scope_test2_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/cmdargs_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/unit_test_core_apply.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/capped_allocator_tests_generated/allocator_test_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/protobuf_writer_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/protobuf_writer_apply.cpp")
add_target_to_folder (core_test_common)
add_dependencies(core_test_common
    unit_test_codegen1
    unit_test_codegen2
    unit_test_codegen3
    unit_test_codegen4)
target_include_directories (core_test_common PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
target_compile_definitions (core_test_common PUBLIC
 #       -DENABLE_TEST_CASE=0x20010000
        -DBOND_COMPACT_BINARY_PROTOCOL
        -DBOND_SIMPLE_BINARY_PROTOCOL
        -DBOND_FAST_BINARY_PROTOCOL
        -DBOND_SIMPLE_JSON_PROTOCOL)
target_link_libraries (core_test_common PUBLIC
    bond
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})


add_bond_codegen (TARGET unit_test_codegen1
    unit_test.bond
    OPTIONS
        --using=\"static_string=std::array<char, {0}>\"
        --using=\"static_wstring=std::array<wchar_t, {0}>\"
        --using=\"simple_list=SimpleList<{0}>\"
        --header=\\\"custom_protocols.h\\\"
        --header=\\\"container_extensibility.h\\\")

add_bond_codegen (TARGET unit_test_codegen2
    unit_test_core.bond
    apply_test.bond
    import_test1.bond
    import_test2.bond
    protobuf_writer.bond
    scope_test1.bond
    scope_test2.bond
    validation.bond
    cmdargs.bond
    OPTIONS
        --header=\\\"custom_protocols.h\\\")

add_bond_codegen (TARGET unit_test_codegen3
    allocator_test.bond
    OPTIONS
        --using=\"Vector=std::vector<{0}, detail::TestAllocator<{0}> >\"
        --header=\\\"custom_protocols.h\\\"
        --header=\\\"allocators.h\\\"
        --allocator=TestAllocator)

add_bond_codegen (TARGET unit_test_codegen4
    allocator_test.bond
    OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/capped_allocator_tests_generated"
    OPTIONS
        --using=\"Vector=std::vector<{0}, bond::capped_allocator<std::allocator<{0}> > >\"
        --header=\"<bond/core/capped_allocator.h>\"
        --allocator=\"bond::capped_allocator<>\"
        --namespace=\"allocator_test=capped_allocator_tests\")

# Full unit test is too big, .pdb files exceed compiler limit;
# Since debugging such large executable is not practical anyways,
# we disable .pdb generation.
# TODO: add support for selecting individual test case via ENABLE_TEST_CASE macro.
no_pdb()


add_unit_test (allocator_test.cpp)
add_unit_test (apply_tests.cpp)
add_unit_test (basic_tests.cpp)
add_unit_test (basic_type_lists.cpp)
add_unit_test (basic_type_map.cpp)
add_unit_test (blob_tests.cpp)
add_unit_test (bonded_tests.cpp)
add_unit_test (capped_allocator_tests.cpp)
add_unit_test (checked_test.cpp)
add_unit_test (cmdargs.cpp)
add_unit_test (container_extensibility.cpp
    associative_container_extensibility.cpp)
add_unit_test (custom_protocols.cpp)
add_unit_test (enum_conversions.cpp)
add_unit_test (exception_tests.cpp)
add_unit_test (generics_test.cpp)
add_unit_test (inheritance_test.cpp)
add_unit_test (json_tests.cpp)
add_unit_test (list_tests.cpp)
add_unit_test (marshal.cpp)
add_unit_test (maybe_tests.cpp)
add_unit_test (may_omit_fields.cpp)
add_unit_test (merge_test.cpp)
add_unit_test (metadata_tests.cpp)
add_unit_test (nullable.cpp)
add_unit_test (numeric_conversions.cpp)
add_unit_test (pass_through.cpp)

add_unit_test (protobuf_writer_test.cpp)
add_dependencies(protobuf_writer_test protobuf_test_codegen)
target_link_libraries(protobuf_writer_test PRIVATE protobuf_test_codegen)

add_unit_test (protocol_test.cpp)
add_unit_test (required_fields_tests.cpp)
add_unit_test (serialization_test.cpp)
add_unit_test (set_tests.cpp)
add_unit_test (skip_id_tests.cpp)
add_unit_test (skip_type_tests.cpp)
add_unit_test (validate_tests.cpp)
