#pragma once

#include <bond/core/validate.h>

#include <boost/static_assert.hpp>

template <typename T> struct
compatible_types
{
    typedef boost::mpl::list<> type;
};

// uint32 -> uint64
template <> struct
compatible_types<uint32_t>
{
    typedef boost::mpl::list<uint64_t> type;
};

// uint16 -> uint32
template <> struct
compatible_types<uint16_t>
{
    typedef boost::mpl::list<uint64_t, uint32_t> type;
};

// uint8 -> uint16
template <> struct
compatible_types<uint8_t>
{
    typedef boost::mpl::list<uint64_t, uint32_t, uint16_t> type;
};

// int32 -> int64
template <> struct
compatible_types<int32_t>
{
    typedef boost::mpl::list<int64_t> type;
};

// int16 -> int32
template <> struct
compatible_types<int16_t>
{
    typedef boost::mpl::list<int64_t, int32_t> type;
};

// int8 -> int16
template <> struct
compatible_types<int8_t>
{
    typedef boost::mpl::list<int64_t, int32_t, int16_t> type;
};

// float -> double
template <> struct
compatible_types<float>
{
    typedef boost::mpl::list<double> type;
};

template <typename T> struct
compatible_types_or_self
{
    typedef typename boost::mpl::push_front<typename compatible_types<T>::type, T>::type type;
};

template <typename T> struct
incompatible_types
{
    typedef typename boost::mpl::remove_if<NumericTypes, boost::mpl::contains<typename compatible_types_or_self<T>::type, _> >::type type;
};

template <typename T1, typename T2>
inline bool ValidateCompatible()
{
    bond::RuntimeSchema src = bond::GetRuntimeSchema<T1>();
    bond::RuntimeSchema dst = bond::GetRuntimeSchema<T2>();

    try
    {
        bond::Validate(src, dst);
        return true;
    }
    catch (const bond::SchemaValidateException&)
    {
        return false;
    }
}

template <typename Reader, typename Writer, typename Num1>
struct NumericConverterFail
{
    template <typename Num2>
    void operator()(const Num2&)
    {
        BOOST_STATIC_ASSERT((!bond::is_matching_basic<Num1, Num2>::value));
        BOOST_STATIC_ASSERT(boost::is_arithmetic<Num1>::value || boost::is_enum<Num1>::value);
        BOOST_STATIC_ASSERT(boost::is_arithmetic<Num2>::value || boost::is_enum<Num2>::value);

        typedef BondStruct<Num1> From;
        typedef BondStruct<Num2> To;

        // Num1 and Num2 are some incompatible types.
        // In order to "compare" them, we are setting one to zero
        // and the other to non-zero.
        UT_AssertIsFalse(ValidateCompatible<From, To>());

        From from;
        from.field = static_cast<Num1>(0);

        bond::bonded<From> bonded(GetBonded<Reader, Writer, From>(from));
        bond::bonded<void> bonded_void(bonded);

        To to;
        to.field = static_cast<Num2>(1);

        UT_AssertIsTrue(!!to.field);
        UT_AssertIsFalse(!!from.field);

        bonded.Deserialize(to);
        UT_AssertIsTrue(!!to.field);

        bonded_void.Deserialize(to);
        UT_AssertIsTrue(!!to.field);
    }
};


template <typename Reader, typename Writer, typename Num1>
struct NumericConverter2
{
    template <typename Num2>
    void operator()(const Num2&)
    {
        BOOST_STATIC_ASSERT((bond::is_matching_basic<Num1, Num2>::value));

        {
            typedef BondStruct<Num1> From;
            typedef BondStruct<Num2> To;

            // "interesting" integer constants
            const std::vector<Num1>& constants = IntegerConstants<Num1>();

            for(size_t i = 0; i < constants.size(); ++i)
            {
                From value;
                value.field = constants[i];

                BindingAndMapping<Reader, Writer, From, To>(value);
            }

            UT_AssertIsTrue(ValidateCompatible<From, To>());
        }

        {
            typedef BondStruct<list<Num1> >   From;
            typedef BondStruct<vector<Num2> > To;

            AllBindingAndMapping<Reader, Writer, From, To>();
            UT_AssertIsTrue(ValidateCompatible<From, To>());
        }

        {
            typedef BondStruct<bond::nullable<Num1> >   From;
            typedef BondStruct<bond::nullable<Num2> >   To;

            AllBindingAndMapping<Reader, Writer, From, To>();
            UT_AssertIsTrue(ValidateCompatible<From, To>());
        }

        {
            typedef BondStruct<std::set<Num1> >   From;
            typedef BondStruct<std::set<Num2> >   To;

            AllBindingAndMapping<Reader, Writer, From, To>();
            UT_AssertIsTrue(ValidateCompatible<From, To>());
        }

        {
            typedef BondStruct<std::map<Num1, uint8_t> >   From;
            typedef BondStruct<std::map<Num2, uint32_t> >  To;

            AllBindingAndMapping<Reader, Writer, From, To>();
            UT_AssertIsTrue(ValidateCompatible<From, To>());
        }

        {
            typedef BondStruct<std::map<int16_t, Num1> >   From;
            typedef BondStruct<std::map<int64_t, Num2> >   To;

            AllBindingAndMapping<Reader, Writer, From, To>();
            UT_AssertIsTrue(ValidateCompatible<From, To>());
        }
    }
};
