include (FindPackageHandleStandardArgs)
include (HaskellUtil)

find_haskell_program (ghc)

find_package_handle_standard_args (GHC
    FOUND_VAR GHC_FOUND
    REQUIRED_VARS Haskell_GHC_EXECUTABLE
    VERSION_VAR GHC_VERSION
    FAIL_MESSAGE 
"Couldn't find the required version of ghc. You can install ghc with Haskell Platform (https://www.haskell.org/platform/). If the right version of ghc is already installed but wasn't found, you can specify the directory where it is located via the environment variable GHC_PATH")

mark_as_advanced (Haskell_GHC_EXECUTABLE)
