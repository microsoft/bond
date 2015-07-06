# Doxygen doesn't allow specifying output directory via command line arguments
# so we need this silly workaround to set environment variable that is then
# used in the .doxygen file.
set (ENV{BOND_DOXYGEN_OUTPUT_DIR} ${Doxygen_OUTPUT_DIR})

execute_process (
    COMMAND ${Doxygen_EXECUTABLE} doxygen/bond.doxygen
    RESULT_VARIABLE error)

if (error)
    message (FATAL_ERROR)
endif() 
