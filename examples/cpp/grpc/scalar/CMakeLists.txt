add_bond_test (grpc-scalar scalar.bond scalar.cpp GRPC)

cxx_target_compile_definitions (MSVC grpc-scalar PRIVATE -D_WIN32_WINNT=0x0600)

target_link_libraries(grpc-scalar PRIVATE grpc++)
target_include_directories(grpc-scalar
  # TODO: It feels like the grpc++ target should have this as part of its
  # interface... Perhaps a patch for upstream?
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../../../thirdparty/grpc/include
)
