add_bond_python_module (python_extension
    extension.cpp
    extension.bond
    example.py)

target_compile_definitions (python_extension PRIVATE
    -DBOND_COMPACT_BINARY_PROTOCOL
    -DBOND_SIMPLE_JSON_PROTOCOL)

add_python_test (
    NAME python_extension
    MODULE python_extension
    SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/example.py)
