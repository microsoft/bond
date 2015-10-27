# warning C4310: cast truncates constant value
cxx_add_compile_options(MSVC /wd4310)

add_subfolder (compat "tests/compat")

if (Boost_UNIT_TEST_FRAMEWORK_FOUND)
    add_subfolder (core "tests/unit_test/core")
endif()
