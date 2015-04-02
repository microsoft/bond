# This is a workaround for Windows quirks.
# CMake Visual Studio generator translates add_custom_command into a batch file
# embedded in Visual Studio project. Batch files have problems with paths that
# contain non-ascii characters because they are limited to DOS encoding. It so
# happens that cabal is quite likely to be installed in such a path because by
# default cabal install uses directory under %APPDATA% which contains user name.
# As a workaround we execute this .cmake script as a custom command and use CMake
# cache to get access to variables set during configuration.

cmake_policy (SET CMP0012 NEW)

if ($ENV{APPVEYOR})
    # AppVeyor Azure VMs have limited memory and shakespeare package doesn't
    # install as a dependency of the Bond compiler. As a workaround we install
    # shakespeare explicitly when running under AppVeyor and we limit the heap
    # used by ghc to 400 MB.
    execute_process (
        COMMAND ${Haskell_CABAL_EXECUTABLE} install shakespeare --with-compiler=${Haskell_GHC_EXECUTABLE} --jobs --ghc-option=+RTS --ghc-option=-M400m --ghc-option=-RTS
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        RESULT_VARIABLE error)
    
    if (error)
        message (FATAL_ERROR)
    endif()
endif()

execute_process (
    COMMAND ${Haskell_CABAL_EXECUTABLE} install   ${cabal_options} --with-compiler=${Haskell_GHC_EXECUTABLE} --extra-prog-path=$ENV{HOME}/.cabal/bin --only-dependencies --jobs
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    RESULT_VARIABLE error)

if (error)
    message (FATAL_ERROR)
endif()

execute_process (
    COMMAND ${Haskell_CABAL_EXECUTABLE} configure ${cabal_options} --with-compiler=${Haskell_GHC_EXECUTABLE} --builddir=${build_dir}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    RESULT_VARIABLE error)

if (error)
    message (FATAL_ERROR)
endif()

execute_process (
    COMMAND ${Haskell_CABAL_EXECUTABLE} build ${target} --with-ghc=${Haskell_GHC_EXECUTABLE} --ghc-option=-O2 --jobs --builddir=${build_dir}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    RESULT_VARIABLE error)

if (error)
    message (FATAL_ERROR)
endif()

