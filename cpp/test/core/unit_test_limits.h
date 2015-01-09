#pragma once

#include <stdint.h>

#ifdef _DEBUG
    const uint32_t c_max_string_length = 20;
    const uint32_t c_max_list_size     = 10;
    const uint32_t c_iterations        = 1;
#else
    const uint32_t c_max_string_length = 50;
    const uint32_t c_max_list_size     = 20;
    const uint32_t c_iterations        = 5;
#endif

