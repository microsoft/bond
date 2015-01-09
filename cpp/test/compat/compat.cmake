function (run)
    execute_process (
        COMMAND ${ARGV}
        RESULT_VARIABLE error)
    if (error)
        message (FATAL_ERROR)
    endif()
endfunction()

set (TEST_CS ${TEST})

# C# implements reading of Compact Binary v2 but not writing
if (${TEST_CS} STREQUAL compact2)
    set (TEST_CS compact)
endif()

run (${CSHARP_COMPAT} ${TEST} ${COMPAT_DATA}/compat.${TEST}.dat compat.${TEST}.cs.dat ${TEST_CS})
run (${BOND_COMPAT} ${TEST_CS} -d compat.${TEST}.cs.dat expected.cs.${TEST} deserialized.cs.${TEST})
