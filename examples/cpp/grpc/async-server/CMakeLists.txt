add_bond_test (grpc-async-server async-server.bond async-server.cpp GRPC)

cxx_target_compile_definitions (MSVC grpc-async-server PRIVATE -D_WIN32_WINNT=0x0600)

# Supress warning about [[deprecated]] Bond-over-gRPC code.
cxx_target_no_warn_deprecated (grpc-async-server)

if (BOND_FIND_GRPC)
    find_package(grpc CONFIG REQUIRED)
    target_link_libraries(grpc-async-server PRIVATE gRPC::grpc++)
else ()
    target_link_libraries(grpc-async-server PRIVATE grpc++)
endif ()
