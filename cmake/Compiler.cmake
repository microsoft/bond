function (cxx_target_compile_options compiler)
    list (REMOVE_AT ARGV 0)
    if (${CMAKE_CXX_COMPILER_ID} STREQUAL ${compiler})
        target_compile_options (${ARGV})
    endif()
endfunction()

function (cxx_target_compile_definitions compiler)
    list (REMOVE_AT ARGV 0)
    if (${CMAKE_CXX_COMPILER_ID} STREQUAL ${compiler})
        target_compile_definitions (${ARGV})
    endif()
endfunction()

function (cxx_add_compile_options compiler)
    list (REMOVE_AT ARGV 0)
    if (${CMAKE_CXX_COMPILER_ID} STREQUAL ${compiler})
        add_compile_options (${ARGV})
    endif()
endfunction()

# TODO: remove when deleting Bond-over-gRPC code
function (cxx_target_no_warn_deprecated target)
    # Need to ignore both the warning about not understanding [[deprecated]]
    # and the warning about [[deprecated]] things if the compiler _does_
    # understand [[deprecated]]
    cxx_target_compile_options (AppleClang ${target} PRIVATE -Wno-unknown-attributes -Wno-deprecated-declarations)
    cxx_target_compile_options (Clang ${target} PRIVATE -Wno-unknown-attributes -Wno-deprecated-declarations)
    cxx_target_compile_options (GNU ${target} PRIVATE -Wno-attributes -Wno-deprecated-declarations)

    cxx_target_compile_options (MSVC ${target} PRIVATE /wd4996)
endfunction()
