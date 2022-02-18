add_bond_test (grpc-pingpong pingpong.bond pingpong.cpp GRPC)

cxx_target_compile_definitions (MSVC grpc-pingpong PRIVATE -D_WIN32_WINNT=0x0600)
# Supress warning about [[deprecated]] Bond-over-gRPC code.
cxx_target_no_warn_deprecated (grpc-pingpong)

if (BOND_FIND_GRPC)
    find_package(grpc CONFIG REQUIRED)
    target_link_libraries(grpc-pingpong PRIVATE gRPC::grpc++)
else ()
    target_link_libraries(grpc-pingpong PRIVATE grpc++)
endif ()
