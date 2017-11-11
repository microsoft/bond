#pragma once

#include <bond/core/container_interface.h>
#include <bond/core/exception.h>

#include <array>
#include <boost/assert.hpp>

namespace bond
{
    //
    // Treat std::array<T, N> as list<T>
    //
    template <typename T, std::size_t N> struct
    is_list_container<std::array<T, N> >
        : std::true_type
    {};

    //
    // Bond custom container interface implementation
    //

    // resize_list
    template <typename T, std::size_t N>
    void resize_list(std::array<T, N>&, uint32_t size)
    {
        if (size > N)
        {
            BOND_THROW(CoreException, "Size " << size << " exceeds static array capacity " << N);
        }
    }

    // Use the default implememenetation of the other functions and traits in
    // container concept.
}

