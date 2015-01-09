#include <iostream>
#include <bond/core/tuple.h>
#include <bond/protocol/simple_json_writer.h>

#define field(Name, Value) std::make_pair(#Name, Value)
#define trace(Event, ...) Trace(#Event, ##__VA_ARGS__)

template <typename I>
void SetFieldNames(I)
{}

template <typename I, typename ...T>
void SetFieldNames(I it, const char* name, const T&...rest)
{
    it->metadata.name = name;
    SetFieldNames(++it, rest...);
}


template <typename ...T1, typename ...T2>
bond::blob Trace(const char* eventName, const std::pair<T1, T2>&...args)
{
    std::tuple<T2...> values(args.second...);

    auto schema = bond::GetRuntimeSchema(values).GetSchema();
    auto& struct_ = schema.structs[schema.root.struct_def];

    struct_.metadata.name = eventName;
    struct_.metadata.qualified_name = eventName;
    SetFieldNames(struct_.fields.begin(), args.first...);
    
    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

    Serialize(schema, writer);
    Serialize(values, writer);

    return output.GetBuffer();
}


int main()
{
    std::string x1 = "banana";
    int x2 = 10;

    // Create Compact Binary reader from trace data
    bond::InputBuffer input = trace(MyEvent, field(Fruit, x1), field(Count, x2));
    bond::CompactBinaryReader<bond::InputBuffer> reader(input);

    // Deserialize schema from the trace data
    bond::bonded<
        bond::SchemaDef, 
        bond::CompactBinaryReader<bond::InputBuffer>&> schemaPayload(reader);
    auto schema = boost::make_shared<bond::SchemaDef>();
    schemaPayload.Deserialize(*schema);
    
    // Transcode trace payload into JSON using the schema to provide names
    bond::bonded<void> tracePayload(reader, schema);
    bond::OutputBuffer output;
    bond::SimpleJsonWriter<bond::OutputBuffer> json(output, true);
    tracePayload.Serialize(json);

    std::string str(output.GetBuffer().content(), output.GetBuffer().length());
    std::cout << str;
    return 0;
}
