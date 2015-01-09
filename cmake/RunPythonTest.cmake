set (ENV{PYTHONPATH} ${PYTHONPATH})

execute_process (
    COMMAND ${PYTHON_EXECUTABLE} ${SCRIPT}
    RESULT_VARIABLE error)

if (error)
    message (FATAL_ERROR)
endif()
