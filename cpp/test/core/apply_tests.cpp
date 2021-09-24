#include "precompiled.h"
#include "apply_tests.h"
#include "apply_test_reflection.h"
#include "apply_test_apply.h"   // Note that pre-generated Apply overloads will be chosen
                                // even though apply_test_reflection.h is included.
#include <boost/range/adaptor/map.hpp>
#include <boost/range/numeric.hpp>

#include <iterator>
#include <map>
#include <sstream>

void Init(unittest::apply::Struct& obj)
{
    obj = InitRandom<unittest::apply::Struct>();
}


void Init(unittest::apply::Derived& obj)
{
    obj = InitRandom<unittest::apply::Derived>();
}


template <typename Reader, typename Writer, typename X>
void Marshal(uint16_t version = bond::v1)
{
    X obj, obj2;

    Init(obj);

    bond::OutputBuffer output;
    Factory<Writer>::Call(output, version, boost::bind(
        bond::Marshal<bond::BuiltInProtocols, X, Writer>, obj, boost::placeholders::_1));

    bond::InputBuffer input = output.GetBuffer();

    Unmarshal(input, obj2);

    UT_Compare(obj, obj2);
}


template <typename Reader, typename Writer, typename X>
void Serialize(uint16_t version = bond::v1)
{
    X obj, obj2;

    Init(obj);

    bond::OutputBuffer output;
    Factory<Writer>::Call(output, version, boost::bind(
        bond::Serialize<bond::BuiltInProtocols, X, Writer>, obj, boost::placeholders::_1));

    bond::InputBuffer input = output.GetBuffer();
    Reader reader(Factory<Reader>::Create(input, version));

    Deserialize(reader, obj2);

    UT_Compare(obj, obj2);
}


template <typename Transform, typename T>
void CallApply(const Transform& transform, const T& value)
{
    Apply(transform, value);
}

template <typename Reader, typename Writer, typename X>
void Apply(uint16_t version = bond::v1)
{
    X obj, obj2;

    Init(obj);

    bond::OutputBuffer output;
    Factory<Writer>::Call(output, version, boost::bind(
        CallApply<bond::Serializer<Writer>, X>, boost::bind(bond::SerializeTo<bond::BuiltInProtocols, Writer>, boost::placeholders::_1), obj));

    bond::InputBuffer input = output.GetBuffer();
    Reader reader(Factory<Reader>::Create(input, version));
    bond::bonded<X> bonded(reader);

    Apply(bond::To<X>(obj2), bonded);

    UT_Compare(obj, obj2);
}


// Just check that Apply overload can be called without attempting roundtrips
template <typename Reader, typename Writer, typename X>
void SimpleApply(uint16_t version = bond::v1)
{
    X obj;

    Init(obj);

    typename Writer::Buffer output;
    Factory<Writer>::Call(output, version, boost::bind(
        CallApply<bond::Serializer<Writer>, X>, boost::bind(bond::SerializeTo<bond::BuiltInProtocols, Writer>, boost::placeholders::_1), obj));
}


template <typename Reader, typename Writer, typename X>
typename boost::enable_if<bond::uses_static_parser<Reader> >::type
Bonded(uint16_t = bond::v1)
{
    // apply_test_reflection.h is necessary for pass-through of untagged
    // protocols such as Simple even when using *_apply because struct skipping 
    // depends on compile-time schema defined in _reflection.h.
}

template <typename Reader, typename Writer, typename X>
typename boost::disable_if<bond::uses_static_parser<Reader> >::type
Bonded(uint16_t version = bond::v1)
{
    X obj, obj2;

    Init(obj);

    bond::OutputBuffer output;
    Factory<Writer>::Call(output, version, boost::bind(
        bond::Serialize<bond::BuiltInProtocols, X, Writer>, obj, boost::placeholders::_1));

    bond::InputBuffer input = output.GetBuffer();
    Reader reader(Factory<Reader>::Create(input, version));
    bond::bonded<X> bonded(reader);

    bond::OutputBuffer output2;
    Factory<Writer>::Call(output2, version, boost::bind(
        &bond::bonded<X>::template Serialize<bond::BuiltInProtocols, Writer>, bonded, boost::placeholders::_1));

    bond::InputBuffer input2 = output2.GetBuffer();
    Reader reader2(Factory<Reader>::Create(input2, version));
    bond::bonded<X> bonded2(reader2);
    
    bonded2.Deserialize(obj2);

    UT_Compare(obj, obj2);
}


class IDLBuilder : public bond::Transform
{
public:
    explicit IDLBuilder(std::map<std::string, std::string>& structs)
        : _structs{ &structs },
          _qualifiedName{},
          _container{ bond::BT_UNAVAILABLE }
    {}

    void Begin(const bond::Metadata& metadata) const
    {
        _qualifiedName = &metadata.qualified_name;

        if (_structs->find(*_qualifiedName) == _structs->end())
            _this << std::boolalpha << "struct " << *_qualifiedName << "\n{\n";
        else
            _structs = {};
    }

    template <typename T>
    bool Base(const T& value) const
    {
        if (!_structs)
            return true;

        _this.seekp(-3, std::ios_base::end);    // Undo the added "\n{\n" in Begin.
        _this << " : " << StructName(value) << "\n{\n";
        return false;
    }

    template <typename T>
    bool Field(uint16_t id, const bond::Metadata& metadata, const T& value) const
    {
        if (!_structs)
            return true;

        _this << "    " << id << ": " << ToString(metadata.modifier) << ' ';
        TypeName(value);
        _this << ' ' << metadata.name;

        if (metadata.default_value.nothing)
            _this << " = nothing";
        else
            DefaultValue<typename bond::remove_bonded_value<T>::type>(metadata.default_value);

        _this << ";\n";
        return false;
    }

    template <typename T>
    void Container(const T& value, std::size_t size) const
    {
        Container(size, value);
    }

    template <typename K, typename T>
    void Container(const K& key, const T& value, std::size_t size) const
    {
        Container(size, key, value);
    }

    template <typename T>
    bool UnknownField(std::uint16_t /*id*/, const T& /*value*/) const BOND_NOEXCEPT
    {
        return !_structs;
    }

    void UnknownEnd() const BOND_NOEXCEPT
    {}

    void End() const
    {
        if (!_structs)
            return;

        _this << "};\n";
        _structs->emplace(*_qualifiedName, _this.str());
    }

private:
    template <typename T>
    const std::string& StructName(const T& value) const
    {
        IDLBuilder that{ *_structs };
        Apply(that, value);
        return *that._qualifiedName;
    }

    template <typename T, typename Reader, typename boost::enable_if<bond::is_basic_type<T> >::type* = nullptr>
    void TypeName(const bond::value<T, Reader>& /*value*/) const
    {
        _this << bond::detail::type<typename std::conditional<std::is_enum<T>::value, std::int32_t, T>::type>::name();
    }

    template <typename T>
    void TypeName(const T& value) const
    {
        _container = bond::GetTypeId(value);

        if (_container == bond::BT_STRUCT)
            _this << StructName(value);
        else
            Apply(*this, value);
    }

    template <typename... T>
    void Container(std::size_t size, const T&... value) const
    {
        BOOST_VERIFY(size == 0);
        assert(bond::BT_LIST <= _container && _container <= bond::BT_MAP);
        assert((sizeof...(T) == 2) == (_container == bond::BT_MAP));

        const char* labels[] = { "list", "set", "map" };
        _this << labels[_container - bond::BT_LIST] << '<';
        (void)std::initializer_list<int>{ (TypeName(value), _this << ", ", 0)... };
        _this.seekp(-2, std::ios_base::end);    // Undo the last ", ".
        _this << '>';
    }

    template <typename T, typename Enable = void> struct
    default_value_field;

    template <typename T> struct
    default_value_field<T, typename boost::enable_if<std::is_unsigned<T> >::type> : std::integral_constant<std::uint64_t bond::Variant::*, &bond::Variant::uint_value> {};

    template <typename T> struct
    default_value_field<T, typename boost::enable_if<bond::is_signed_int_or_enum<T> >::type> : std::integral_constant<std::int64_t bond::Variant::*, &bond::Variant::int_value> {};

    template <typename T> struct
    default_value_field<T, typename boost::enable_if<std::is_floating_point<T> >::type> : std::integral_constant<double bond::Variant::*, &bond::Variant::double_value> {};

    template <typename T> struct
    default_value_field<T, typename boost::enable_if<bond::is_string<T> >::type> : std::integral_constant<std::string bond::Variant::*, &bond::Variant::string_value> {};

    template <typename T> struct
    default_value_field<T, typename boost::enable_if<bond::is_wstring<T> >::type> : std::integral_constant<std::wstring bond::Variant::*, &bond::Variant::wstring_value> {};

    template <typename T>
    void FormatValue(const T& value) const
    {
        _this << value;
    }

    void FormatValue(const std::wstring& value) const
    {
        std::transform(value.begin(), value.end(), std::ostreambuf_iterator<char>(_this), [](wchar_t wc) { return char(wc); });
    }

    template <typename T, typename boost::enable_if<bond::is_basic_type<T> >::type* = nullptr>
    void DefaultValue(const bond::Variant& var) const
    {
        const auto& value = var.*default_value_field<T>::value;
        if (value == decltype(value){})
            return;

        _this << " = ";
        if (bond::is_string_type<T>::value)
            _this << '"';

#if defined(_MSC_VER) && _MSC_VER < 1910
#pragma warning(push)
#pragma warning(disable: 4800)  // warning C4800: 'const uint64_t': forcing value to bool 'true' or 'false'
#endif
        FormatValue(static_cast<T>(value));
#if defined(_MSC_VER) && _MSC_VER < 1910
#pragma warning(pop)
#endif
        if (bond::is_string_type<T>::value)
            _this << '"';
    }

    template <typename T, typename boost::disable_if<bond::is_basic_type<T> >::type* = nullptr>
    void DefaultValue(const bond::Variant& /*var*/) const BOND_NOEXCEPT
    {}

    mutable std::map<std::string, std::string>* _structs;
    mutable const std::string* _qualifiedName;
    mutable std::ostringstream _this;
    mutable bond::BondDataType _container;
};


struct ApplySchemaTests
{
    template <typename T>
    void operator()(const T&)
    {
        bond::RuntimeSchema schema = bond::GetRuntimeSchema<T>();

        std::map<std::string, std::string> structs1;
        Apply<T>(IDLBuilder{ structs1 });
        auto idl1 = boost::accumulate(structs1 | boost::adaptors::map_values, std::string{});
        UT_AssertAreEqual(structs1.size(), schema.GetSchema().structs.size());

        std::map<std::string, std::string> structs2;
        Apply(IDLBuilder{ structs2 }, schema);
        auto idl2 = boost::accumulate(structs2 | boost::adaptors::map_values, std::string{});
        UT_AssertAreEqual(structs2.size(), schema.GetSchema().structs.size());

        UT_AssertAreEqual(idl1, idl2);
    }
};


template <typename Reader, typename Writer>
struct Tests
{
    template <typename X>
    void operator()(const X&)
    {
        Marshal<Reader, Writer, X>();
        Marshal<Reader, Writer, X>(Reader::version);
        
        Serialize<Reader, Writer, X>();
        Serialize<Reader, Writer, X>(Reader::version);

        Apply<Reader, Writer, X>();
        Apply<Reader, Writer, X>(Reader::version);

        Bonded<Reader, Writer, X>();
        Bonded<Reader, Writer, X>(Reader::version);
    }
};

// Partial specialization for special CompactBinary internal output counter
template <typename Reader>
struct Tests<Reader, bond::CompactBinaryWriter<bond::OutputBuffer>::Pass0>
{
    template <typename X>
    void operator()(const X&)
    {
        SimpleApply<Reader, bond::CompactBinaryWriter<bond::OutputBuffer>::Pass0, X>();
        SimpleApply<Reader, bond::CompactBinaryWriter<bond::OutputBuffer>::Pass0, X>(Reader::version);
    }
};


typedef boost::mpl::list<
    unittest::apply::Struct,
    unittest::apply::Derived
> Types;

template <typename Reader, typename Writer>
TEST_CASE_BEGIN(AllTests)
{
    boost::mpl::for_each<Types>(Tests<Reader, Writer>());
}
TEST_CASE_END

TEST_CASE_BEGIN(SchemaTests)
{
    boost::mpl::for_each<Types>(ApplySchemaTests{});
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void ApplyTests(const char* name)
{
    UnitTestSuite suite(name);
    
    AddTestCase<TEST_ID(N), AllTests, Reader, Writer>(suite, "Use generated *_apply.cpp");
}

template <uint16_t N>
void ApplyTests(const char* name)
{
    UnitTestSuite suite(name);

    AddTestCase<TEST_ID(N), SchemaTests>(suite, "Apply transform to compile-time and runtime schema");
}


void ApplyTest::Initialize()
{
    TEST_SIMPLE_PROTOCOL(
        ApplyTests<
            0x1601,
            bond::SimpleBinaryReader<bond::InputBuffer>,
            bond::SimpleBinaryWriter<bond::OutputBuffer> >("Apply tests for SimpleBinary");
    );

    TEST_COMPACT_BINARY_PROTOCOL(
        ApplyTests<
            0x1602,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer> >("Apply tests for CompactBinary");
    );

    // Test for apply overloads for CompactBinary v2 Pass0 output size counter
    TEST_COMPACT_BINARY_PROTOCOL(
        ApplyTests<
            0x1603,
            bond::CompactBinaryReader<bond::InputBuffer>,
            bond::CompactBinaryWriter<bond::OutputBuffer>::Pass0 >("Apply tests for CompactBinary v2 Pass0");
    );

    TEST_FAST_BINARY_PROTOCOL(
        ApplyTests<
            0x1604,
            bond::FastBinaryReader<bond::InputBuffer>,
            bond::FastBinaryWriter<bond::OutputBuffer> >("Apply tests for FastBinary");
    );

    ApplyTests<0x1605>("Apply tests for compile-time and runtime schemas");
}


bool init_unit_test()
{
    ApplyTest::Initialize();
    return true;
}

