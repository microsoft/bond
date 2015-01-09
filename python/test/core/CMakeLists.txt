add_bond_python_module (bond_python_unit_test
    unit_test.cpp
    unit_test.bond
    unit_test.py)

target_compile_definitions (bond_python_unit_test PRIVATE
    -DBOND_COMPACT_BINARY_PROTOCOL)

# disable generation of debug symbols to speed up build
no_pdb()

add_python_test (
    NAME bond_python_unit_test
    MODULE bond_python_unit_test
    SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/unit_test.py)
