# This is a dummy project to make sure that gRPC++ builds and can be linked
# correctly. This will be removed later, when we have a Bond-over-gRPC++
# test.

add_bond_test (grpc-dummy g.bond g.cpp)

cxx_target_compile_definitions (MSVC grpc-dummy PRIVATE -D_WIN32_WINNT=0x0600)

target_link_libraries(grpc-dummy PRIVATE grpc++)
target_include_directories(grpc-dummy
  # TODO: It feels like the grpc++ target should have this as part of its
  # interface... Perhaps a patch for upstream?
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../../thirdparty/grpc/include
)
