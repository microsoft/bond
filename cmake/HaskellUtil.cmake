macro (find_haskell_program program)
    string (TOUPPER ${program} PROGRAM)
    if (NOT ${PROGRAM}_FOUND)
        set (_HASKELL_PLATFORM_VERSIONS 
            2014.2.0.0 
            2013.2.0.0 
            2012.4.0.0 
            2012.2.0.0)
        
        foreach (_VER ${_HASKELL_PLATFORM_VERSIONS})
            list (APPEND _HASKELL_PLATFORM_PATHS 
                "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Haskell\\Haskell Platform\\${_VER};InstallDir]")
        endforeach()

        unset (Haskell_${PROGRAM}_EXECUTABLE CACHE)
        find_program (Haskell_${PROGRAM}_EXECUTABLE ${program}
            HINTS $ENV{${PROGRAM}_PATH}
            PATH_SUFFIXES bin
            NO_DEFAULT_PATH
            PATHS  
                "$ENV{HOME}/.cabal"
                "$ENV{HOME}/Library/Haskell"
                "$ENV{APPDATA}/cabal"
                ${_HASKELL_PLATFORM_PATHS}
                ENV PATH
                ${CMAKE_SYSTEM_PROGRAM_PATH})

        if (Haskell_${PROGRAM}_EXECUTABLE)
            execute_process (
                COMMAND "${Haskell_${PROGRAM}_EXECUTABLE}" --numeric-version
                OUTPUT_VARIABLE ${PROGRAM}_VERSION
                ERROR_QUIET)
            string (REGEX REPLACE "\n" "" ${PROGRAM}_VERSION "${${PROGRAM}_VERSION}")
        endif()
        unset (_HASKELL_PLATFORM_VERSIONS)
        unset (_HASKELL_PLATFORM_PATHS)
    endif()
endmacro()    
