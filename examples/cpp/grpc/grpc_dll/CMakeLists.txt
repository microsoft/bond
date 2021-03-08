add_bond_codegen (grpc_dll.bond
    OPTIONS --header=\\\"dllexample_dynlink.h\\\" --apply=compact --export-attribute=DLLEXAMPLE_DYNLINK
    GRPC)

add_library (grpc_dll_example_lib EXCLUDE_FROM_ALL
    SHARED
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/grpc_dll_types.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/grpc_dll_apply.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/grpc_dll_grpc.cpp)

target_include_directories(grpc_dll_example_lib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_compile_definitions (grpc_dll_example_lib PRIVATE -DBUILDING_DLLEXAMPLE_DLL)

add_target_to_folder (grpc_dll_example_lib)

if (BOND_FIND_GRPC)
    find_package(grpc CONFIG REQUIRED)
    target_link_libraries(grpc_dll_example_lib PUBLIC
        gRPC::grpc++)
else ()
    target_link_libraries(grpc_dll_example_lib PUBLIC
        grpc++)
endif ()

target_link_libraries (grpc_dll_example_lib PUBLIC
    bond)

cxx_target_compile_definitions (MSVC grpc_dll_example_lib PRIVATE
    -D_WIN32_WINNT=0x0600)

add_bond_test (grpc_dll_example
    using_grpc_dll.cpp)

target_compile_definitions (grpc_dll_example PRIVATE -DUSING_DLLEXAMPLE_DLL)

target_include_directories(grpc_dll_example PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries (grpc_dll_example PUBLIC
    grpc_dll_example_lib)

cxx_target_compile_definitions (MSVC grpc_dll_example PRIVATE
    -D_WIN32_WINNT=0x0600)
