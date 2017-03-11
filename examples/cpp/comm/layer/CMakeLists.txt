add_bond_test (layer layer.bond layer.cpp COMM)

cxx_target_compile_definitions (MSVC layer PRIVATE -D_WIN32_WINNT=0x0600)

# Deep in the guts of Boost we get a warning about not using a "secure"
# routine. Suppress them for this example.
cxx_target_compile_options(MSVC layer PRIVATE -D_SCL_SECURE_NO_WARNINGS)
