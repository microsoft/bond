set (core_schemas
    ${BOND_IDL}/bond/core/bond.bond
    ${BOND_IDL}/bond/core/bond_const.bond)

set (schemas)
list (APPEND schemas ${core-schemas})

add_bond_codegen (${core_schemas}
    ENUM_HEADER
    OUTPUT_DIR ${BOND_GENERATED}/bond/core)

# Generate again into intermediate dir so that we can build libraries
# concurrently.
# Despite what documentation says, there doesn't seem to be a way to build
# multiple targets from one set of generated files without serializing them.
# Even when using add_custom_target to define an intermediate target the custom
# command rules are added to each target and VS forces execution of all rules
# in a clean build, leading to collisions in multi-processor build.
add_bond_codegen (${core_schemas}
    ENUM_HEADER
    OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bond/core)

file (GLOB core_headers "inc/bond/core/*.h")
source_group ("core" FILES ${core_headers})

file (GLOB core_detail_headers "inc/bond/core/detail/*.h")
source_group ("core\\detail" FILES ${core_detail_headers})

file (GLOB protocol_headers "inc/bond/protocol/*.h")
source_group ("protocol" FILES ${protocol_headers})

file (GLOB protocol_detail_headers "inc/bond/protocol/detail/*.h")
source_group ("protocol\\detail" FILES ${protocol_detail_headers})

file (GLOB stream_headers "inc/bond/stream/*.h")
source_group ("stream" FILES ${stream_headers})

set (generated_files_types)
list (APPEND generated_files_types
    ${BOND_GENERATED}/bond/core/bond_types.cpp
    ${BOND_GENERATED}/bond/core/bond_const_types.cpp)

set (generated_files_apply)
list (APPEND generated_files_apply
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bond/core/bond_apply.cpp)

set (precompiled_sources)
list (APPEND precompiled_sources
    "src/bond/core/detail/typeid_value.cpp"
    "src/bond/core/parser.cpp"
    "src/bond/core/select_protocol.cpp"
    "src/bond/core/value.cpp"
    "src/bond/protocol/detail/rapidjson_utils.cpp")

list (APPEND headers
    ${core_headers}
    ${core_detail_headers}
    ${protocol_headers}
    ${protocol_detail_headers}
    ${stream_headers})

source_group ("generated" FILES ${generated_files_types} ${generated_files_apply})

add_library (bond
    STATIC
    ${schemas}
    ${generated_files_types}
    ${precompiled_sources}
    ${headers})

target_compile_definitions (bond PUBLIC -DBOND_LIB_TYPE=BOND_LIB_TYPE_STATIC)

# Boost thread is only needed if the C++ standard library doesn't support
# std::call_once. However std::once seems problematic on Linux
# (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=60662) so for now we use
# std::call_once only on MSVC.
if (Boost_SYSTEM_FOUND AND Boost_THREAD_FOUND OR NOT MSVC)
    target_link_libraries (bond INTERFACE
        ${Boost_SYSTEM_LIBRARY}
        ${Boost_THREAD_LIBRARY})
endif()

add_library (bond_apply
    STATIC
    ${schemas}
    ${generated_files_apply})

target_link_libraries (bond_apply bond)
target_include_directories (bond_apply BEFORE PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})

install (TARGETS bond bond_apply
    EXPORT bond
    ARCHIVE DESTINATION lib/bond
    INCLUDES DESTINATION include)

install (DIRECTORY ${BOND_IDL}/bond/core DESTINATION include/bond)

if (BOND_LIBRARIES_INSTALL_CPP)
  install (
    DIRECTORY ${BOND_GENERATED}/bond/
    DESTINATION src/bond/generated
    FILES_MATCHING PATTERN "*.cpp")

  install (
    DIRECTORY ${BOND_SOURCE_DIR}/
    DESTINATION src/
    FILES_MATCHING PATTERN "*.cpp")
endif()

add_subdirectory (test)
