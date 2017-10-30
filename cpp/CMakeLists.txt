set (core_schemas
    ${BOND_IDL}/bond/core/bond.bond
    ${BOND_IDL}/bond/core/bond_const.bond)

set (schemas)
list (APPEND schemas ${core-schemas})

if (BOND_ENABLE_COMM)
    set(comm-flag COMM)
endif()

if (BOND_ENABLE_GRPC)
    set(grpc-flag GRPC)
endif()

add_bond_codegen (${core_schemas}
    ENUM_HEADER
    ${comm-flag} ${grpc-flag}
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
    ${comm-flag} ${grpc-flag}
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

list (APPEND headers
    ${core_headers}
    ${core_detail_headers}
    ${protocol_headers}
    ${protocol_detail_headers}
    ${stream_headers})

if (BOND_ENABLE_COMM)
    set (comm_schemas
        ${BOND_IDL}/bond/comm/comm.bond
        ${BOND_IDL}/bond/comm/transport/packet.bond
        ${BOND_IDL}/bond/comm/transport/epoxy_transport.bond)

    list (APPEND schemas ${comm-schemas})

    add_bond_codegen (${comm_schemas}
        ENUM_HEADER
        OUTPUT_DIR ${BOND_GENERATED}/bond/comm)

    add_bond_codegen (${comm_schemas}
        ENUM_HEADER
        OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bond/comm)

    file (GLOB comm_headers "inc/bond/comm/*.h")
    source_group ("comm" FILES ${comm_headers})

    file (GLOB comm_detail_headers "inc/bond/comm/detail/*.h")
    source_group ("comm\\detail" FILES ${comm_detail_headers})

    file (GLOB comm_layers_headers "inc/bond/comm/layers/*.h")
    source_group ("comm\\layers" FILES ${comm_layers_headers})

    file (GLOB comm_transport_headers "inc/bond/comm/transport/*.h")
    source_group ("comm\\transport" FILES ${comm_transport_headers})

    file (GLOB comm_transport_detail_headers "inc/bond/comm/transport/detail/*.h")
    source_group ("comm\\transport\\detail" FILES ${comm_transport_detail_headers})

    list (APPEND generated_files_types
        ${BOND_GENERATED}/bond/comm/comm_types.cpp
        ${BOND_GENERATED}/bond/comm/packet_types.cpp
        ${BOND_GENERATED}/bond/comm/epoxy_transport_types.cpp)

    list (APPEND generated_files_apply
        ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bond/comm/comm_apply.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bond/comm/packet_apply.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bond/comm/epoxy_transport_apply.cpp)

    list (APPEND headers
        ${comm_headers}
        ${comm_detail_headers}
        ${comm_layers_headers}
        ${comm_transport_headers}
        ${comm_transport_detail_headers})
endif(BOND_ENABLE_COMM)

# ext headers (currently, only used by gRPC stuff, but in its own block
# because it doesn't depend on gRPC stuff)
if (BOND_ENABLE_GRPC)
    file (GLOB ext_detail_headers "inc/bond/ext/detail/*.h")
    source_group ("ext\\detail" FILES ${ext_detail_headers})

    list (APPEND headers
        ${ext_detail_headers})
endif()

# gRPC-specific headers
if (BOND_ENABLE_GRPC)
    file (GLOB ext_grpc_headers "inc/bond/ext/grpc/*.h")
    source_group ("ext\\grpc" FILES ${ext_grpc_headers})

    file (GLOB ext_grpc_detail_headers "inc/bond/ext/grpc/detail/*.h")
    source_group ("ext\\grpc\\detail" FILES ${ext_grpc_detail_headers})

    list (APPEND headers
        ${ext_detail_headers}
        ${ext_grpc_headers}
        ${ext_grpc_detail_headers})
endif()

source_group ("generated" FILES ${generated_files_types} ${generated_files_apply})

add_library (bond
    STATIC
    ${schemas}
    ${generated_files_types}
    ${headers})

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

if (BOND_ENABLE_COMM)
    install (DIRECTORY ${BOND_IDL}/bond/comm DESTINATION include/bond)
endif()

if (BOND_LIBRARIES_INSTALL_CPP)
  install (
    DIRECTORY ${BOND_GENERATED}/bond/
    DESTINATION src/bond/generated
    FILES_MATCHING PATTERN "*.cpp")
endif()

add_subdirectory (test)
