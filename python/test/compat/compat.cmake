set (ENV{PYTHONPATH} ${PYTHON_COMPAT})

function (run)
    execute_process (
        COMMAND ${ARGV}
        RESULT_VARIABLE error)
    if (error)
        message (FATAL_ERROR)
    endif()
endfunction()

run (${PYTHON_EXECUTABLE} ${SOURCE_DIR}/compat.py -d ${COMPAT_DATA}/compat.${TEST}.dat -s compat.${TEST}.py.dat ${TEST})
run (${BOND_COMPAT} ${TEST} -d compat.${TEST}.py.dat expected.py.${TEST} deserialized.py.${TEST})
