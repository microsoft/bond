#pragma once

#include <boost/config.hpp>
#include <boost/multiprecision/cpp_int.hpp>
#include <bond/core/container_interface.h>

using namespace boost::multiprecision;

namespace bond
{
    //
    // Treat Boost multiprecision integers as Bond blobs (i.e. list<int8>)
    //
    template <
        unsigned MinBits,
        unsigned MaxBits,
        cpp_integer_type SignType,
        cpp_int_check_type Checked,
        typename Allocator,
        expression_template_option ExpressionTemplates> struct
    is_list_container<
        number<
            backends::cpp_int_backend<MinBits, MaxBits, SignType, Checked, Allocator>,
            ExpressionTemplates> >
        : std::true_type
    {};

    template <
        unsigned MinBits,
        unsigned MaxBits,
        cpp_integer_type SignType,
        cpp_int_check_type Checked,
        typename Allocator,
        expression_template_option ExpressionTemplates> struct
    element_type<
        number<
            backends::cpp_int_backend<MinBits, MaxBits, SignType, Checked, Allocator>,
            ExpressionTemplates> >
    {
        typedef int8_t type;
    };

    //
    // Bond custom container interface implementation
    //

    // resize_list
    template <
        unsigned MinBits,
        unsigned MaxBits,
        cpp_integer_type SignType,
        cpp_int_check_type Checked,
        typename Allocator,
        expression_template_option ExpressionTemplates>
    void resize_list(
            number<backends::cpp_int_backend<MinBits, MaxBits, SignType, Checked, Allocator>, ExpressionTemplates>& num,
            uint32_t size)
    {
        num.backend().resize(size / sizeof(limb_type), size / sizeof(limb_type));
    }


    // container_size
    template <
        unsigned MinBits,
        unsigned MaxBits,
        cpp_integer_type SignType,
        cpp_int_check_type Checked,
        typename Allocator,
        expression_template_option ExpressionTemplates>
    uint32_t container_size(
            const number<backends::cpp_int_backend<MinBits, MaxBits, SignType, Checked, Allocator>, ExpressionTemplates>& num)
    {
        if (num == 0)
            // Bond expects that blob with default value is empty
            return 0;
        else
            return num.backend().size() * sizeof(limb_type);
    }


    // const_enumerators
    template <
        unsigned MinBits,
        unsigned MaxBits,
        cpp_integer_type SignType,
        cpp_int_check_type Checked,
        typename Allocator,
        expression_template_option ExpressionTemplates>
    class const_enumerator<
        number<
            backends::cpp_int_backend<MinBits, MaxBits, SignType, Checked, Allocator>,
            ExpressionTemplates> >
    {
    public:
        const_enumerator(const number<backends::cpp_int_backend<MinBits, MaxBits, SignType, Checked, Allocator> >& num)
            : it(static_cast<const int8_t*>(static_cast<const void*>(num.backend().limbs()))),
              end(it + num.backend().size() * sizeof(limb_type))
        {}

        bool more() const
        {
            return it != end;
        }

        int8_t next()
        {
            return *(it++);
        }

    private:
        const int8_t *it, *end;
    };


    // enumerator
    template <
        unsigned MinBits,
        unsigned MaxBits,
        cpp_integer_type SignType,
        cpp_int_check_type Checked,
        typename Allocator,
        expression_template_option ExpressionTemplates>
    class enumerator<
        number<
            backends::cpp_int_backend<MinBits, MaxBits, SignType, Checked, Allocator>,
            ExpressionTemplates> >
    {
    public:
        enumerator(number<backends::cpp_int_backend<MinBits, MaxBits, SignType, Checked, Allocator> >& num)
            : it(static_cast<int8_t*>(static_cast<void*>(num.backend().limbs()))),
              end(it + num.backend().size() * sizeof(limb_type))
        {}

        bool more() const
        {
            return it != end;
        }

        int8_t& next()
        {
            return *(it++);
        }

    private:
        int8_t *it, *end;
    };
}

