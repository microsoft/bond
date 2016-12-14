#pragma once

#if defined(BUILDING_DLLEXAMPLE_DLL)
    #define DLLEXAMPLE_DYNLINK __declspec(dllexport)
#elif defined(USING_DLLEXAMPLE_DLL)
    #define DLLEXAMPLE_DYNLINK __declspec(dllimport)
#else
    #define DLLEXAMPLE_DYNLINK
#endif
