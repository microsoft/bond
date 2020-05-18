#pragma once

#include "unit_test_limits.h"
#include "unit_test_framework.h"

#include <bond/core/bond.h>
#include <bond/core/config.h>
#include <bond/stream/output_buffer.h>
#include <bond/protocol/simple_json_writer.h>
#include <bond/protocol/random_protocol.h>

using bond::container_size;
using bond::string_length;

#include "equal.h"
#include "moved.h"

#include <boost/mpl/copy.hpp>
#include <boost/assign/list_of.hpp>
#include <boost/bind/bind.hpp>
#include <boost/function.hpp>

using namespace std;
using boost::mpl::_;

typedef boost::mpl::list
    <
#ifndef UNIT_TEST_TYPE_SUBSET
        uint8_t, uint16_t, uint32_t, uint64_t,
        int8_t,   int16_t,  int32_t,  int64_t,
        bool, float, double
#else
        int32_t, double
#endif
    >
    NumericTypes;

typedef boost::mpl::list
    <
#ifndef UNIT_TEST_TYPE_SUBSET
        wstring,
#endif
        string
    >
    StringTypes;

typedef boost::mpl::copy
    <
        StringTypes,
        boost::mpl::front_inserter<NumericTypes>
    >::type
    SortableTypes;

typedef boost::mpl::push_front
    <
        SortableTypes,
        bond::blob
    >::type
    BasicTypes;


template <typename T>
struct ListTypes
{
    typedef boost::mpl::list
    <
#ifndef UNIT_TEST_TYPE_SUBSET
        list<T>,
        vector<T>
#else
        vector<T>
#endif
    > type;
};


template <typename T>
struct SkipTypes
{
    typedef boost::mpl::list
    <
#ifndef UNIT_TEST_TYPE_SUBSET
        BondStruct<T>,
        vector<list<T> >,
        //list<T>,
        set<T>,
        map<string, T>,
        //BondStruct<list<T> >,
        bond::nullable<T>,
        bond::nullable<BondStruct<T> >,
        vector<BondStruct<list<T> > >
#else
        vector<BondStruct<list<T> > >
#endif
    > type;
};


template <typename T> struct
is_protocols
    : std::false_type {};

template <typename... T> struct
is_protocols<bond::Protocols<T...>>
    : std::true_type {};

template <> struct
is_protocols<bond::BuiltInProtocols>
    : std::true_type {};


template <typename T>
const std::vector<T>& IntegerConstants()
{
#ifdef _MSC_VER
    #pragma warning (push)
    // warning C4310: cast truncates constant value
    #pragma warning (disable: 4310)
#endif

    static std::vector<T> constants = boost::assign::list_of
        ((T)-1)((T)0)((T)1)((T)2)((T)3)((T)4)((T)5)((T)6)((T)7)((T)8)((T)9)
        ((T)SCHAR_MAX)((T)SCHAR_MIN)((T)UCHAR_MAX)
        ((T)SHRT_MIN)((T)SHRT_MAX)((T)USHRT_MAX)
        ((T)INT_MIN)((T)INT_MAX)((T)UINT_MAX)
        ((T)LLONG_MAX)((T)LLONG_MIN)((T)ULLONG_MAX);

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

    return constants;
}


// CreateSelfMappings creates mappings compatible with MapTo<T> transform
// for every field of the specified bond structure.
template <typename Protocols>
class CreateSelfMappings
    : public bond::SerializingTransform
{
public:
    CreateSelfMappings(bond::Mappings& mappings)
        : _mappings(mappings)
    {}

    CreateSelfMappings(bond::Mappings& mappings, bond::Path path)
        : _mappings(mappings),
          _path(path)
    {}

    void Begin(const bond::Metadata& /*metadata*/) const
    {}

    void End() const
    {}

    template <typename T>
    bool Base(const T& value) const
    {
        _path.push_back(bond::mapping_base);
        bond::Apply<Protocols>(CreateSelfMappings(_mappings[bond::mapping_base].fields, _path), value);
        _path.pop_back();
        return false;
    }

    template <typename T>
    typename boost::disable_if<bond::is_bond_type<T>, bool>::type
    Field(uint16_t id, const bond::Metadata& /*metadata*/, const T&) const
    {
        _path.push_back(id);
        _mappings[id].path = _path;
        _path.pop_back();

        return false;
    }

    template <typename T>
    typename boost::enable_if<bond::is_bond_type<T>, bool>::type
    Field(uint16_t id, const bond::Metadata& /*metadata*/, const T& value) const
    {
        _path.push_back(id);
        bond::Apply<Protocols>(CreateSelfMappings(_mappings[id].fields, _path), value);
        _path.pop_back();

        return false;
    }

    template <typename T>
    bool Field(uint16_t id, const bond::Metadata& /*metadata*/, const bond::bonded<T>& /*value*/) const
    {
        _path.push_back(id);
        _mappings[id].path = _path;
        _path.pop_back();

        return false;
    }

private:
    bond::Mappings& _mappings;
    mutable bond::Path _path;
};


template <typename Protocol>
struct Factory
{
    static Protocol Create(typename Protocol::Buffer& buffer, uint16_t /*version*/)
    {
        return Protocol(buffer);
    }

    static void Call(typename Protocol::Buffer& buffer, uint16_t /*version*/, boost::function<void (Protocol&)> func)
    {
       Protocol proto(buffer);
       return func(proto);
    }
};


template <typename Buffer>
struct Factory<bond::SimpleBinaryReader<Buffer> >
{
    static bond::SimpleBinaryReader<Buffer> Create(Buffer& buffer, uint16_t version)
    {
        return bond::SimpleBinaryReader<Buffer>(buffer, version);
    }
};


template <typename Buffer>
struct Factory<bond::CompactBinaryReader<Buffer> >
{
    static bond::CompactBinaryReader<Buffer> Create(Buffer& buffer, uint16_t version)
    {
        return bond::CompactBinaryReader<Buffer>(buffer, version);
    }
};


template <typename Buffer>
struct Factory<bond::SimpleJsonWriter<Buffer> >
{
    static void Call(Buffer& buffer, uint16_t /*version*/, boost::function<void (bond::SimpleJsonWriter<Buffer>&)> func)
    {
        bond::SimpleJsonWriter<Buffer> writer(buffer, true, 4, false);
        return func(writer);
    }
};


template <typename Buffer>
struct Factory<bond::SimpleBinaryWriter<Buffer> >
{
    static void Call(Buffer& buffer, uint16_t version, boost::function<void (bond::SimpleBinaryWriter<Buffer>&)> func)
    {
       bond::SimpleBinaryWriter<Buffer> writer(buffer, version);
       return func(writer);
    }
};


template <typename Buffer>
struct Factory<bond::CompactBinaryWriter<Buffer> >
{
    static void Call(Buffer& buffer, uint16_t version, boost::function<void (bond::CompactBinaryWriter<Buffer>&)> func)
    {
        bond::CompactBinaryWriter<Buffer> writer(buffer, version);
        return func(writer);
    }
};


template <typename Reader, typename Writer, typename Protocols = bond::BuiltInProtocols, typename T>
Reader Serialize(const T& x, uint16_t version = bond::v1)
{
    typename Writer::Buffer output_buffer(4096);

    // serialize value to output
    Factory<Writer>::Call(output_buffer, version, boost::bind(
        bond::Serialize<Protocols, T, Writer>, x, boost::placeholders::_1));

    typename Reader::Buffer input_buffer(output_buffer.GetBuffer());
    return Factory<Reader>::Create(input_buffer, version);
}


template <typename Reader, typename Writer, typename Protocols = bond::BuiltInProtocols, typename From, typename To>
typename boost::disable_if<bond::uses_static_parser<Reader> >::type
SerializeDeserialize(const From& from, To& to, uint16_t version = bond::v1)
{
    bond::Deserialize<Protocols>(Serialize<Reader, Writer, Protocols>(from, version), to);
}


template <typename Reader, typename Writer, typename Protocols = bond::BuiltInProtocols, typename From, typename To>
typename boost::enable_if<bond::uses_static_parser<Reader> >::type
SerializeDeserialize(const From& from, To& to, uint16_t version = bond::v1)
{
    bond::bonded<void>(Serialize<Reader, Writer, Protocols>(from, version), bond::GetRuntimeSchema<From>()).template Deserialize<Protocols>(to);
}



template <typename Reader, typename Writer, typename Protocols = bond::BuiltInProtocols, typename Payload, typename T>
Reader Merge(const Payload& payload, const T& x, uint16_t version = bond::v1)
{
    typename Writer::Buffer output_buffer;

    // merge x with serialized payload into output
    Factory<Writer>::Call(output_buffer, version, boost::bind(
        bond::Merge<Protocols, T, Reader, Writer>, x, Serialize<Reader, Writer, Protocols>(payload, version), boost::placeholders::_1));

    typename Reader::Buffer input_buffer(output_buffer.GetBuffer());

    return Factory<Reader>::Create(input_buffer, version);
 }


template <typename Reader, typename Writer, typename BondedType, typename Protocols = bond::BuiltInProtocols, typename T>
bond::bonded<BondedType> GetBonded(T x, uint16_t version = bond::v1)
{
    return bond::bonded<BondedType>(Serialize<Reader, Writer, Protocols>(x, version));
}


template <typename Protocols = bond::BuiltInProtocols, typename T>
void InitRandom(T& x, uint32_t max_string_length = c_max_string_length, uint32_t max_list_size = c_max_list_size)
{
    // Instead of deserializing directly from RandomProtocolReader to instance of T
    // we transcode from Random protocol to Fast protocol and then deserialize
    // the Fast protocol payload into the instance of T. This obvioulsy take longer
    // at runtime, but it compiles faster; build time is the bottleneck for unit tests.

    bond::RandomProtocolReader                      random_reader(max_string_length, max_list_size);
    bond::bonded<void, bond::RandomProtocolReader&> random(random_reader, bond::GetRuntimeSchema<T>());

    bond::OutputBuffer                              buffer(1024);
    bond::FastBinaryWriter<bond::OutputBuffer>     writer(buffer);

    random.template Serialize<Protocols>(writer);

    bond::FastBinaryReader<bond::InputBuffer>      reader(buffer.GetBuffer());

    bond::Deserialize<Protocols>(reader, x);
}


template <typename T>
void Fixup(T&)
{}


template <typename T>
void Fixup(SkipStruct<T>& x)
{
    // For SkipStruct we set the field1 to default so that we can validate that not only it
    // was skipped but that the value of the field was not changed as side effect of skipping.
    x.field1 = T();
}


template <typename T, typename Protocols = bond::BuiltInProtocols>
T InitRandom(uint32_t max_string_length = c_max_string_length, uint32_t max_list_size = c_max_list_size)
{
    T x;

    InitRandom<Protocols>(x, max_string_length, max_list_size);

    return x;
}


template <typename Field> struct
is_optional_field
    : std::is_same<typename Field::field_modifier, bond::reflection::optional_field_modifier> {};


template <typename Protocols = bond::BuiltInProtocols, typename T>
void CopyAndMove(const T& src)
{
    T x(src);
    UT_Equal_P(x, src, Protocols);

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    T y(std::move(x));
    UT_Equal_P(y, src, Protocols);
    UT_AssertIsTrue(moved(x));
#endif
}


template <typename Reader, typename Writer, typename From, typename To, typename BondedType, typename Protocols = bond::BuiltInProtocols>
void Binding(const From& from, uint16_t version = bond::v1)
{
    BOOST_STATIC_ASSERT((std::is_same<BondedType, From>::value
                      || !bond::uses_static_parser<Reader>::value));

    // Compile-time schema
    {
        bond::bonded<BondedType> bonded(GetBonded<Reader, Writer, BondedType, Protocols>(from, version));

        To to;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4127) // C4127: conditional expression is constant
#endif
        if (boost::mpl::count_if<typename From::Schema::fields, is_optional_field<_> >::value == 0)
#ifdef _MSC_VER
#pragma warning(pop)
#endif
        {
            to = InitRandom<To, Protocols>();
            Fixup(to);
        }

        bond::Apply<Protocols>(bond::To<To, Protocols>(to), bonded);

        UT_Equal_P(from, to, Protocols);
    }

    // Runtime schema
    {
        bond::bonded<void> bonded(GetBonded<Reader, Writer, BondedType, Protocols>(from, version));

        To to;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4127) // C4127: conditional expression is constant
#endif
        if (boost::mpl::count_if<typename From::Schema::fields, is_optional_field<_> >::value == 0)
#ifdef _MSC_VER
#pragma warning(pop)
#endif
        {
            to = InitRandom<To, Protocols>();
            Fixup(to);
        }

        bond::Apply<Protocols>(bond::To<To, Protocols>(to), bonded);

        UT_Equal_P(from, to, Protocols);
    }

    CopyAndMove<Protocols>(from);
}


template <typename To, typename BondedType, typename Protocols = bond::BuiltInProtocols>
typename boost::enable_if_c<(bond::detail::hierarchy_depth<typename BondedType::Schema>::value
                          == bond::detail::hierarchy_depth<typename To::Schema>::value)>::type
InitMappings(bond::Mappings& mappings)
{
    To to;
    bond::Apply<Protocols>(CreateSelfMappings<Protocols>(mappings), to);
}


template <typename To, typename BondedType, typename Protocols = bond::BuiltInProtocols>
typename boost::enable_if_c<(bond::detail::hierarchy_depth<typename BondedType::Schema>::value
                           > bond::detail::hierarchy_depth<typename To::Schema>::value)>::type
InitMappings(bond::Mappings& mappings)
{
    // The data we are deserializing has deeper hierarchy than the target variable
    // so we will only map fields of the base struct, and recurse to next level.
    InitMappings<To, typename BondedType::Schema::base, Protocols>(mappings[bond::mapping_base].fields);
}


template <typename Reader, typename Writer, typename From, typename To, typename BondedType, typename Protocols = bond::BuiltInProtocols>
void Mapping(const From& from, uint16_t version = bond::v1)
{
#ifdef UNIT_TEST_MAPPING
    BOOST_STATIC_ASSERT((std::is_same<BondedType, From>::value
                      || !bond::uses_static_parser<Reader>::value));

    bond::Mappings mappings;

    InitMappings<To, BondedType>(mappings);

    // Compile-time schema
    {
        bond::bonded<BondedType> bonded(GetBonded<Reader, Writer, BondedType, Protocols>(from, version));

        To to;

        if (boost::mpl::count_if<From::Schema::fields, is_optional_field<_> >::value == 0)
        {
            to = InitRandom<To, Protocols>();
            Fixup(to);
        }

        bond::Apply<Protocols>(bond::MapTo<To, Protocols>(to, mappings), bonded);

        UT_Equal_P(from, to, Protocols);
    }

    // Runtime schema
    {
        bond::bonded<void> bonded(GetBonded<Reader, Writer, BondedType, Protocols>(from, version));

        To to;

        if (boost::mpl::count_if<From::Schema::fields, is_optional_field<_> >::value == 0)
        {
            to = InitRandom<To, Protocols>();
            Fixup(to);
        }

        bond::Apply<Protocols>(bond::MapTo<To, Protocols>(to, mappings), bonded);

        UT_Equal_P(from, to, Protocols);
    }
#else
    (void)from;
    (void)version;
#endif
}


template <typename Reader, typename Writer, typename From, typename To, typename BondedType, typename Protocols = bond::BuiltInProtocols>
void AllBinding()
{
    // default value
    Binding<Reader, Writer, From, To, BondedType, Protocols>(From());
    Binding<Reader, Writer, From, To, BondedType, Protocols>(From(), Reader::version);

    // random values
    for (uint32_t i = 0; i < c_iterations; ++i)
    {
        Binding<Reader, Writer, From, To, BondedType, Protocols>(InitRandom<From, Protocols>());
        Binding<Reader, Writer, From, To, BondedType, Protocols>(InitRandom<From, Protocols>(), Reader::version);
    }
}


template <typename Reader, typename Writer, typename From, typename To, typename BondedType, typename Protocols = bond::BuiltInProtocols>
void AllMapping()
{
    // default value
    Mapping<Reader, Writer, From, To, BondedType, Protocols>(From());
    Mapping<Reader, Writer, From, To, BondedType, Protocols>(From(), Reader::version);

    // random values
    for (uint32_t i = 0; i < c_iterations; ++i)
    {
        Mapping<Reader, Writer, From, To, BondedType, Protocols>(InitRandom<From, Protocols>());
        Mapping<Reader, Writer, From, To, BondedType, Protocols>(InitRandom<From, Protocols>(), Reader::version);
    }
}


template <typename Reader, typename Writer, typename From, typename To, typename Protocols = bond::BuiltInProtocols>
typename boost::disable_if<bond::uses_static_parser<Reader> >::type
BindingAndMapping(const From& from)
{
    Binding<Reader, Writer, From, To, From, Protocols>(from);
    Mapping<Reader, Writer, From, To, From, Protocols>(from);

    // For dynamic parser the schema doesn't have to exactly match the payload
    Binding<Reader, Writer, From, To, To, Protocols>(from);
    Mapping<Reader, Writer, From, To, To, Protocols>(from);
}


template <typename Reader, typename Writer, typename From, typename To, typename Protocols = bond::BuiltInProtocols>
typename boost::enable_if<bond::uses_static_parser<Reader> >::type
BindingAndMapping(const From& from)
{
    Binding<Reader, Writer, From, To, From, Protocols>(from);
    Mapping<Reader, Writer, From, To, From, Protocols>(from);
}


template <typename Reader, typename Writer, typename T, typename Protocols = bond::BuiltInProtocols>
typename boost::enable_if_c<!is_protocols<T>::value && is_protocols<Protocols>::value>::type
AllBindingAndMapping()
{
    AllBinding<Reader, Writer, T, T, T, Protocols>();
    AllMapping<Reader, Writer, T, T, T, Protocols>();
}


template <typename Reader, typename Writer, typename From, typename To, typename Protocols = bond::BuiltInProtocols>
typename boost::enable_if_c<!bond::uses_static_parser<Reader>::value && !is_protocols<To>::value && is_protocols<Protocols>::value>::type
AllBindingAndMapping()
{
    AllBinding<Reader, Writer, From, To, From, Protocols>();
    AllMapping<Reader, Writer, From, To, From, Protocols>();

    // For dynamic parser the schema doesn't have to exactly match the payload
    AllBinding<Reader, Writer, From, To, To, Protocols>();
    AllMapping<Reader, Writer, From, To, To, Protocols>();
}


template <typename Reader, typename Writer, typename From, typename To, typename Protocols = bond::BuiltInProtocols>
typename boost::enable_if_c<bond::uses_static_parser<Reader>::value && !is_protocols<To>::value && is_protocols<Protocols>::value>::type
AllBindingAndMapping()
{
    AllBinding<Reader, Writer, From, To, From, Protocols>();
    AllMapping<Reader, Writer, From, To, From, Protocols>();
}


template <typename Reader, typename Writer, typename From, typename To, typename BondedType, typename Protocols = bond::BuiltInProtocols>
typename boost::enable_if_c<!bond::uses_static_parser<Reader>::value && !is_protocols<BondedType>::value && is_protocols<Protocols>::value>::type
AllBindingAndMapping()
{
    AllBinding<Reader, Writer, From, To, BondedType, Protocols>();
    AllMapping<Reader, Writer, From, To, BondedType, Protocols>();
}


template <typename Reader, typename Writer, typename From, typename To, typename BondedType, typename Protocols = bond::BuiltInProtocols>
typename boost::enable_if_c<bond::uses_static_parser<Reader>::value && !is_protocols<BondedType>::value && is_protocols<Protocols>::value>::type
AllBindingAndMapping()
{
    // This function is noop for static parser.
    // Calling it with BondedType equal to From or To is most likely unintended
    BOOST_STATIC_ASSERT((!std::is_same<BondedType, From>::value
                      && !std::is_same<BondedType, To>::value));
}


// Unit test wrappers for AllBindingAndMapping

template <typename Reader, typename Writer, typename T, typename Protocols = bond::BuiltInProtocols>
TEST_CASE_BEGIN(AllBindingAndMapping1)
{
    AllBindingAndMapping<Reader, Writer, T, Protocols>();
}
TEST_CASE_END

template <typename Reader, typename Writer, typename From, typename To, typename Protocols = bond::BuiltInProtocols>
TEST_CASE_BEGIN(AllBindingAndMapping2)
{
    AllBindingAndMapping<Reader, Writer, From, To, Protocols>();
}
TEST_CASE_END

template <typename Reader, typename Writer, typename From, typename To, typename BondedType, typename Protocols = bond::BuiltInProtocols>
TEST_CASE_BEGIN(AllBindingAndMapping3)
{
    AllBindingAndMapping<Reader, Writer, From, To, BondedType, Protocols>();
}
TEST_CASE_END
