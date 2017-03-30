add_bond_test (grpc-helloworld helloworld.bond helloworld.cpp GRPC)

cxx_target_compile_definitions (MSVC grpc-helloworld PRIVATE -D_WIN32_WINNT=0x0600)

target_link_libraries(grpc-helloworld PRIVATE grpc++)
target_include_directories(grpc-helloworld
  # TODO: It feels like the grpc++ target should have this as part of its
  # interface... Perhaps a patch for upstream?
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../../thirdparty/grpc/include
)
