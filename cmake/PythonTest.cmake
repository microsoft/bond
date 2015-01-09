set (BOND_CMAKE_DIR ${CMAKE_CURRENT_LIST_DIR})

macro (add_python_subdirectory dir)
    if (PYTHONINTERP_FOUND AND
        PYTHONLIBS_FOUND AND
        Boost_PYTHON_FOUND AND
        (WINDOWSSDK_FOUND OR NOT WIN32))
        add_subdirectory (${dir})
    endif()
endmacro()

function (add_python_test)
    set (flagArgs)
    set (oneValueArgs NAME MODULE SCRIPT)
    set (multiValueArgs)
    cmake_parse_arguments (arg "${flagArgs}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    if (PYTHONINTERP_FOUND)
        add_test (
            NAME ${arg_NAME}
            COMMAND
                ${CMAKE_COMMAND}
                    -DPYTHON_EXECUTABLE=${PYTHON_EXECUTABLE}
                    -DPYTHONPATH=$<TARGET_FILE_DIR:${arg_MODULE}>
                    -DSCRIPT=${arg_SCRIPT}
                    -P ${BOND_CMAKE_DIR}/RunPythonTest.cmake
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    endif()
endfunction()
