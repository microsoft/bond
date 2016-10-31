#include "compat_reflection.h"
#include "compat_no_generics_reflection.h"
#include "compat.h"
#include "compare.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#include <bond/protocol/simple_json_writer.h>
#include <bond/protocol/random_protocol.h>
#include <boost/bind.hpp>
#include <boost/assign/list_of.hpp>
#include <climits>

using namespace unittest::compat;


void Init(std::string& str)
{
    str =
        "Arabic: \xD9\x85\xD8\xB1\xD8\xAD\xD8\xA8\xD8\xA7 \xD8\xA7\xD9\x84\xD8\xB9\xD8\xA7\xD9\x84\xD9\x85 | "
        "Chinese: \xE4\xBD\xA0\xE5\xA5\xBD\xE4\xB8\x96\xE7\x95\x8C | "
        "Hebrew: \xD7\xA9\xD7\x9C\xD7\x95\xD7\x9D \xD7\xA2\xD7\x95\xD7\x9C\xD7\x9D | "
        "Japanese: \xE3\x81\x93\xE3\x82\x93\xE3\x81\xAB\xE3\x81\xA1\xE3\x81\xAF\xE4\xB8\x96\xE7\x95\x8C | "
        "Russian: \xD0\x9F\xD1\x80\xD0\xB8\xD0\xB2\xD0\xB5\xD1\x82 \xD0\xBC\xD0\xB8\xD1\x80 | "
        "Escaped: \" \\ / \b \f \n \r \t \x1 \x1f \x0";
}

void Init(std::wstring& wstr)
{
    wstr =
        L"Arabic: \x0645\x0631\x062d\x0628\x0627 \x0627\x0644\x0639\x0627\x0644\x0645 | "
        L"Chinese: \x4f60\x597d\x4e16\x754c | "
        L"Hebrew: \x05e9\x05dc\x05d5\x05dd \x05e2\x05d5\x05dc\x05dd | "
        L"Japanese: \x3053\x3093\x306b\x3061\x306f\x4e16\x754c | "
        L"Russian: \x041f\x0440\x0438\x0432\x0435\x0442 \x043c\x0438\x0440 | "
        L"Escaped: \" \\ / \b \f \n \r \t \x0001 \x001f \x0";
}


template <typename T>
void Init(std::list<T>& x)
{
    // For smaller types, push the whole range. For larger types, only interesting values.
    bool smallType = (sizeof(T) <= 2);
    if (smallType)
    {
        for (T i = std::numeric_limits<T>::min(); i < std::numeric_limits<T>::max(); i++)
        {
            x.push_back(i);
        }
        x.push_back(std::numeric_limits<T>::max());
    }
    else
    {
        static std::vector<T> constants = boost::assign::list_of
            ((T)-1)((T)0)((T)1)((T)2)((T)3)((T)4)((T)5)((T)6)((T)7)((T)8)((T)9)
            ((T)SCHAR_MAX)((T)SCHAR_MIN)((T)UCHAR_MAX)
            ((T)SHRT_MIN)((T)SHRT_MAX)((T)USHRT_MAX)
            ((T)INT_MIN)((T)INT_MAX)((T)UINT_MAX)
            ((T)LLONG_MAX)((T)LLONG_MIN)((T)ULLONG_MAX);

        std::copy(constants.begin(), constants.end(), std::back_inserter(x));
    }
}

class InitEnums
    : public bond::ModifyingTransform
{
public:
    InitEnums(bond::RandomProtocolReader& random)
        : _random(random)
    {}

    void Begin(const bond::Metadata&) const
    {}

    void End() const
    {}

    void UnknownEnd() const
    {}

    template <typename T>
    bool Base(T& value) const
    {
        Apply(*this, value);
        return false;
    }

    template <typename T>
    bool Field(uint16_t, const bond::Metadata&, T& value) const
    {
        Init(value);
        return false;
    }

    template <typename T>
    bool Field(uint16_t, const bond::Metadata&, bond::maybe<T>& value) const
    {
        if (!value.is_nothing())
            Init(value.value());
        return false;
    }

private:
    // basic type
    template <typename T>
    typename boost::enable_if_c<bond::is_basic_type<T>::value && !std::is_enum<T>::value>::type
    Init(T&) const
    {}

    // enum
    template <typename T>
    typename boost::enable_if<std::is_enum<T> >::type
    Init(T& value) const
    {
        auto enums = GetValueToNameMap(T());
        auto it = enums.begin();
        uint32_t n;

        _random.Read(n);
        std::advance(it, n % enums.size());
        value = it->first;
    }

    // struct value
    template <typename T>
    typename boost::enable_if<bond::is_bond_type<T> >::type
    Init(T& value) const
    {
        Apply(*this, value);
    }

    // bonded<T>
    template <typename T>
    void Init(bond::bonded<T>&) const
    {}

    // 2-tuple
    template <typename T1, typename T2>
    void Init(std::pair<T1, T2>& value) const
    {
        Init(value.second);
    }

    // container
    template <typename T>
    typename boost::enable_if<bond::is_container<T> >::type
    Init(T& value) const
    {
        for (bond::enumerator<T> items(value); items.more();)
        {
            Init(items.next());
        }
    }

    // blob
    void Init(bond::blob&) const
    {}

    // set of enums
    template <typename T>
    typename boost::enable_if<std::is_enum<T> >::type
    Init(std::set<T>& set) const
    {
        bond::clear_set(set);
        for (auto it = GetValueToNameMap(T()).begin(); it != GetValueToNameMap(T()).end(); ++it)
            bond::set_insert(set, it->first);
    }

    template <typename T>
    typename boost::disable_if<std::is_enum<T> >::type
    Init(std::set<T>&) const
    {}

    // map from enums
    template <typename K, typename T>
    typename boost::enable_if<std::is_enum<K>, bool>::type
    Init(std::map<K, T>& map) const
    {
        std::map<K, T> tmp;
        map.swap(tmp);
        auto it = GetValueToNameMap(K()).begin();
        auto it2 = tmp.begin();
        for (; it != GetValueToNameMap(K()).end() && it2 != tmp.end(); ++it, ++it2)
        {
            Init(bond::mapped_at(map, it->first) = it2->second);
        }
        return false;
    }

    bond::RandomProtocolReader& _random;
};


void Init(Test test, Compat& obj)
{
    // Random values
    bond::RandomProtocolReader random(50, 50, test == json);

    // The struct Compat contains a field m_defaults of type nullable<Compat>.
    // If we initialized through RandomProtocolReader directly the result would
    // be infinitely deep object hierarchy (RandomProtocolReader always makes
    // non-empty containers). To prevent the initialization of m_defaults we
    // remove it from the runtime schema.
    bond::SchemaDef schema = bond::GetRuntimeSchema<Compat>().GetSchema();

    // Fields of Compat struct
    std::vector<bond::FieldDef>& fields = schema.structs[schema.root.struct_def].fields;

    struct FieldIdEqual
    {
        bool operator()(uint16_t id, const bond::FieldDef& field)
        {
            return field.id == id;
        }
    };

    // Erase field m_defaults
    fields.erase(
        std::remove_if(fields.begin(), fields.end(), boost::bind<bool>(FieldIdEqual(), Compat::Schema::var::m_defaults::id, _1)),
        fields.end());

    {
        bond::OutputBuffer buffer;
        bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);
        bond::bonded<void, bond::RandomProtocolReader&>(random, bond::RuntimeSchema(schema)).Serialize(writer);
        bond::CompactBinaryReader<bond::InputBuffer> reader(buffer.GetBuffer());
        bond::Deserialize(reader, obj);
    }

    assert(!obj.m_defaults);

    // For tests other than 'simple' initialize m_defaults to an empty instance.
    // For 'simple' we leave m_defaults as null becase an empty instance will
    // contain fields set to 'nothing' which can't be serialized using Simple
    // Protocol.
    if (test != simple && test != simple2)
    {
        bond::OutputBuffer buffer;
        bond::CompactBinaryWriter<bond::OutputBuffer> writer(buffer);
        Serialize(BasicTypesUninitialized(), writer);
        bond::CompactBinaryReader<bond::InputBuffer> reader(buffer.GetBuffer());

        obj.m_defaults.set();
        obj.m_defaults->m_basicUnintialized = bond::bonded<BasicTypesUninitialized>(reader);
    }

    // "interesting" integer constants
    Init(obj.m_containers.l_int8);
    Init(obj.m_containers.l_int16);
    Init(obj.m_containers.l_int32);
    Init(obj.m_containers.l_int64);
    Init(obj.m_containers.l_uint8);
    Init(obj.m_containers.l_uint16);
    Init(obj.m_containers.l_uint32);

    if (test != json)
    {
        // NewtonsoftJson used by C# implementation doesn't support
        // numbers larger that max int64.
        Init(obj.m_containers.l_uint64);
    }

    // "interesting" chars and international strings
    Init(obj.m_basicTypes.m_str);
    Init(obj.m_basicTypes.m_wstr);

    // Fix enums to use only valid enum values
    bond::Apply(InitEnums(random), obj);
}


template <typename Writer, typename T>
bond::blob Serialize(const T& obj)
{
    typename Writer::Buffer buffer;
    Writer writer(buffer);

    bond::Serialize(obj, writer);

    return buffer.GetBuffer();
}


template <typename Writer, typename T, typename Param>
bond::blob Serialize(const T& obj, Param param)
{
    typename Writer::Buffer buffer;
    Writer writer(buffer, param);

    bond::Serialize(obj, writer);

    return buffer.GetBuffer();
}


template <typename Writer, typename T>
bond::blob Marshal(const T& obj)
{
    typename Writer::Buffer buffer;
    Writer writer(buffer);

    bond::Marshal(obj, writer);

    return buffer.GetBuffer();
}


bond::blob Serialize(Test test, const Compat& obj)
{
    switch (test)
    {
        case compact:
            return Serialize<bond::CompactBinaryWriter<bond::OutputBuffer> >(obj);

        case compact2:
            return Serialize<bond::CompactBinaryWriter<bond::OutputBuffer> >(obj, bond::v2);

        case fast:
            return Serialize<bond::FastBinaryWriter<bond::OutputBuffer> >(obj);

        case json:
            return Serialize<bond::SimpleJsonWriter<bond::OutputBuffer> >(obj);

        case simple:
            return Serialize<bond::SimpleBinaryWriter<bond::OutputBuffer> >(obj);

        case simple2:
            return Serialize<bond::SimpleBinaryWriter<bond::OutputBuffer> >(obj, bond::v2);

        case schema:
            return Marshal<bond::CompactBinaryWriter<bond::OutputBuffer> >(bond::GetRuntimeSchema<Compat>());

        default:
            BOOST_ASSERT(false);
            return bond::blob();
    }
}


template <typename Reader, typename T>
void Deserialize(const bond::blob& buffer, T& obj)
{
    T obj2;
    Reader reader(buffer);

    Deserialize(reader, obj);
    Deserialize(reader, obj2, bond::GetRuntimeSchema<T>());

    BOOST_VERIFY(Equal(obj2, obj));
}


template <typename Reader, typename T>
void Deserialize(const bond::blob& buffer, T& obj, uint16_t version)
{
    T obj2;
    Reader reader(buffer, version);

    Deserialize(reader, obj);
    Deserialize(reader, obj2, bond::GetRuntimeSchema<T>());

    BOOST_VERIFY(Equal(obj2, obj));
}

void Deserialize(const bond::blob& buffer, bond::SchemaDef& schema)
{
    if (buffer.content()[0] == '{')
        Deserialize(bond::SimpleJsonReader<bond::InputBuffer>(buffer), schema);
    else
        Unmarshal(bond::InputBuffer(buffer), schema);
}

void Deserialize(Test test, const bond::blob& buffer, Compat& obj, bond::SchemaDef& schema_def)
{
    switch (test)
    {
        case compact:
            return Deserialize<bond::CompactBinaryReader<bond::InputBuffer> >(buffer, obj);

        case compact2:
            return Deserialize<bond::CompactBinaryReader<bond::InputBuffer> >(buffer, obj, bond::v2);

        case fast:
            return Deserialize<bond::FastBinaryReader<bond::InputBuffer> >(buffer, obj);

        case json:
            return Deserialize<bond::SimpleJsonReader<bond::InputBuffer> >(buffer, obj);

        case simple:
            return Deserialize<bond::SimpleBinaryReader<bond::InputBuffer> >(buffer, obj);

        case simple2:
            return Deserialize<bond::SimpleBinaryReader<bond::InputBuffer> >(buffer, obj, bond::v2);

        case schema:
            return Deserialize(buffer, schema_def);

        default:
            BOOST_ASSERT(false);
    }
}


void Convert(const Compat& from, CompatNoGenerics& to)
{
    Deserialize<bond::CompactBinaryReader<bond::InputBuffer> >(
        Serialize<bond::CompactBinaryWriter<bond::OutputBuffer> >(from), to);
}
