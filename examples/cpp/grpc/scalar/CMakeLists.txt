add_bond_test (grpc-scalar scalar.bond scalar.cpp GRPC)

cxx_target_compile_definitions (MSVC grpc-scalar PRIVATE -D_WIN32_WINNT=0x0600)

if(BOND_FIND_GRPC)
    find_package(grpc CONFIG REQUIRED)
    target_link_libraries(grpc-scalar PRIVATE gRPC::grpc++)
else()
    target_link_libraries(grpc-scalar PRIVATE grpc++)
endif()
