#include "precompiled.h"
#include "apply_tests.h"
#include "apply_test_reflection.h"
#include "apply_test_apply.h"   // Note that pre-generated Apply overloads will be chosen
                                // even though apply_test_reflection.h is included.
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


class BuildIDL : public bond::Transform
{
public:
    explicit BuildIDL(std::ostream& out)
        : _out{ out }
    {}

    template <typename T>
    bool Base(const T& value) const
    {
        GetTypeName(value);
        return false;
    }

    void Begin(const bond::Metadata& metadata) const
    {
        _qualifiedName = metadata.qualified_name;
        _this << "struct " << _qualifiedName << "\n{\n";
    }

    template <typename T>
    bool Field(uint16_t id, const bond::Metadata& metadata, const T& value) const
    {
        _this << "    " << id << ": " << ToString(metadata.modifier) << " ";
        _this << GetTypeName(value);
        _this << " " << metadata.name;

        if (metadata.default_value.nothing)
        {
            _this << " = nothing";
        }
        else
        {
            DefaultValue<typename bond::remove_bonded_value<T>::type>(metadata.default_value);
        }

        _this << ";\n";
        return false;
    }

    template <typename T, typename Reader>
    void Container(const bond::value<T, Reader>& value, std::size_t size) const
    {
        BOOST_VERIFY(size == 0);

        switch (_container)
        {
        case bond::BT_LIST:
            _this << "list";
            break;
        case bond::BT_SET:
            _this << "set";
        default:
            assert(false);
            break;
        }

        _this << "<" << GetTypeName(value) << ">";
    }

    template <typename K, typename T, typename Reader>
    void Container(const bond::value<K, Reader>& key, const bond::value<T, Reader>& value, std::size_t size) const
    {
        BOOST_VERIFY(size == 0);

        switch (_container)
        {
        case bond::BT_MAP:
            _this << "map";
            break;
        default:
            assert(false);
            break;
        }

        _this << "<" << GetTypeName(key) << ", " << GetTypeName(value) << ">";
    }

    template <typename T>
    bool UnknownField(std::uint16_t /*id*/, const T& /*value*/) const noexcept
    {
        return false;
    }

    void UnknownEnd() const noexcept
    {}

    void End() const
    {
        _this << "};\n";
        _out << _this.rdbuf();
    }

private:
    template <typename T, typename Reader, typename boost::enable_if_c<bond::is_basic_type<T>::value && !std::is_enum<T>::value>::type* = nullptr>
    std::string GetTypeName(const bond::value<T, Reader>& /*value*/) const
    {
        return bond::detail::type<T>::name();
    }

    template <typename T, typename Reader, typename boost::enable_if<std::is_enum<T> >::type* = nullptr>
    std::string GetTypeName(const bond::value<T, Reader>& /*value*/) const
    {
        return bond::detail::type<std::int32_t>::name();
    }

    template <typename T, typename Reader, typename boost::disable_if<bond::is_basic_type<T> >::type* = nullptr>
    std::string GetTypeName(const bond::value<T, Reader>& value) const
    {
        const auto type = bond::GetTypeId(value);
        if (type == bond::BT_STRUCT)
        {
            BuildIDL that{ _out };
            Apply(that, value);
            return that._qualifiedName;
        }
        else
        {
            _container = type;
            Apply(*this, value);
            return {};
        }
    }

    template <typename T, typename Reader>
    std::string GetTypeName(const bond::bonded<T, Reader>& value) const
    {
        BuildIDL that{ _out };
        Apply(that, value);
        return that._qualifiedName;
    }

    template <typename T, typename boost::enable_if<std::is_same<T, bool> >::type* = nullptr>
    void DefaultValue(const bond::Variant& value) const
    {
        if (value.uint_value != T{})
            _this << " = " << std::boolalpha << bool(value.uint_value);
    }

    template <typename T, typename boost::enable_if_c<std::is_unsigned<T>::value && !std::is_same<T, bool>::value>::type* = nullptr>
    void DefaultValue(const bond::Variant& value) const
    {
        if (value.uint_value != T{})
            _this << " = " << value.uint_value;
    }

    template <typename T, typename boost::enable_if<bond::is_signed_int_or_enum<T> >::type* = nullptr>
    void DefaultValue(const bond::Variant& value) const
    {
        if (value.int_value != T{})
            _this << " = " << value.int_value;
    }

    template <typename T, typename boost::enable_if<std::is_floating_point<T> >::type* = nullptr>
    void DefaultValue(const bond::Variant& value) const
    {
        if (value.double_value != T{})
            _this << " = " << value.double_value;
    }

    template <typename T, typename boost::enable_if<bond::is_string<T> >::type* = nullptr>
    void DefaultValue(const bond::Variant& value) const
    {
        if (value.string_value != T{})
            _this << " = " << value.string_value;
    }

    template <typename T, typename boost::enable_if<bond::is_wstring<T> >::type* = nullptr>
    void DefaultValue(const bond::Variant& value) const
    {
        if (value.wstring_value != T{})
        {
            std::string str(value.wstring_value.size(), ' ');
            std::transform(value.wstring_value.begin(), value.wstring_value.end(), str.begin(), [](wchar_t wc) { return char(wc); });
            _this << " = " << str;
        }
    }

    template <typename T, typename boost::disable_if<bond::is_basic_type<T> >::type* = nullptr>
    void DefaultValue(const bond::Variant& /*value*/) const
    {}

    std::ostream& _out;
    mutable std::string _qualifiedName;
    mutable std::stringstream _this;
    mutable bond::BondDataType _container;
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

        bond::RuntimeSchema schema = bond::GetRuntimeSchema<X>();

        std::ostringstream s1;
        Apply<X>(BuildIDL{ s1 });
        std::ostringstream s2;
        Apply(BuildIDL{ s2 }, schema);
        std::string idl1 = s1.str();
        std::string idl2 = s2.str();
        UT_AssertAreEqual(idl1, idl2);
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


template <typename Reader, typename Writer>
TEST_CASE_BEGIN(AllTests)
{
    typedef boost::mpl::list<
        unittest::apply::Struct,
        unittest::apply::Derived
    > Types;

    boost::mpl::for_each<Types>(Tests<Reader, Writer>());
}
TEST_CASE_END


template <uint16_t N, typename Reader, typename Writer>
void ApplyTests(const char* name)
{
    UnitTestSuite suite(name);
    
    AddTestCase<TEST_ID(N), AllTests, Reader, Writer>(suite, "Use generated *_apply.cpp");
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
}


bool init_unit_test()
{
    ApplyTest::Initialize();
    return true;
}

