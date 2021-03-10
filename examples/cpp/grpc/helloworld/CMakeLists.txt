add_bond_test (grpc-helloworld helloworld.bond helloworld.cpp GRPC)

cxx_target_compile_definitions (MSVC grpc-helloworld PRIVATE -D_WIN32_WINNT=0x0600)

if (BOND_FIND_GRPC)
    find_package(grpc CONFIG REQUIRED)
    target_link_libraries(grpc-helloworld PRIVATE gRPC::grpc++)
else ()
    target_link_libraries(grpc-helloworld PRIVATE grpc++)
endif ()
