add_bond_python_module (python_unit_test
    unit_test.cpp
    unit_test.bond
    unit_test.py)

target_compile_definitions (python_unit_test PRIVATE
    -DBOND_COMPACT_BINARY_PROTOCOL)

# disable generation of debug symbols to speed up build
no_pdb()

add_python_test (
    NAME python_unit_test
    MODULE python_unit_test
    SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/unit_test.py)
