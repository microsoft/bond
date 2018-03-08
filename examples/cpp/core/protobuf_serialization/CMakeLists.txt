add_bond_test (protobuf_serialization protobuf_serialization.bond
    protobuf_serialization.cpp
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/protobuf_serialization.pb.cc")

add_protobuf_codegen_cpp (protobuf_serialization)

target_include_directories (protobuf_serialization PUBLIC
    "${CMAKE_SOURCE_DIR}/thirdparty/protobuf/src"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}")

target_link_libraries(protobuf_serialization PRIVATE libprotobuf)

if (MSVC)
    # C4100: unreferenced formal parameter
    # C4125: decimal digit terminates octal escape sequence
    # C4146: unary minus operator applied to unsigned type, result still unsigned (Fixed in https://github.com/google/protobuf/pull/4000)
    # C4456: declaration of 'size' hides previous local declaration
    # C4505: unreferenced local function has been removed
    # C4510: default constructor could not be generated
    # C4610: struct 'google::protobuf::Map...::Node' can never be instantiated - user defined constructor required
    # C4800: forcing value to bool 'true' or 'false' (performance warning)
    target_compile_options(protobuf_serialization PUBLIC /wd4100 /wd4125 /wd4146 /wd4456 /wd4505 /wd4510 /wd4610 /wd4800)
endif()
