#include <iostream>
#include "input_file.h"
#include "cmd_arg_reflection.h"
#include <bond/core/cmdargs.h>
#include <bond/stream/stdio_output_stream.h>
#include <bond/protocol/simple_json_writer.h>

using namespace bf;

inline bool IsValidType(bond::BondDataType type)
{
    return type >= bond::BT_BOOL && type <= bond::BT_WSTRING;
}

template <typename Reader>
bool TryProtocol(Reader reader, int confidence = 5)
{
    try
    {
        bond::BondDataType type;
        uint32_t size;
        uint16_t id;

        reader.ReadStructBegin();
        reader.ReadFieldBegin(type, id);

        for (int i = 0; i < confidence; ++i, reader.ReadFieldEnd(), reader.ReadFieldBegin(type, id))
        {
            if (type == bond::BT_STOP)
                break;

            if (type == bond::BT_STOP_BASE)
                continue;

            if (!IsValidType(type))
                return false;

            if (type == bond::BT_SET || type == bond::BT_LIST)
            {
                bond::BondDataType element_type;

                Reader(reader).ReadContainerBegin(size, element_type);

                if (!IsValidType(element_type))
                    return false;
            }

            if (type == bond::BT_MAP)
            {
                std::pair<bond::BondDataType, bond::BondDataType> element_type;

                Reader(reader).ReadContainerBegin(size, element_type);

                if (!IsValidType(element_type.first) || !IsValidType(element_type.second))
                    return false;
            }

            reader.Skip(type);
        }

        return true;
    }
    catch(const bond::StreamException&)
    {
        return false;
    }
}


Protocol Guess(InputFile input)
{
    uint16_t word;
    bond::CompactBinaryReader<InputFile> cbp(input);
    bond::FastBinaryReader<InputFile>   mbp(input);
    bond::CompactBinaryReader<InputFile> cbp2(input, bond::v2);

    input.Read(word);

    if (word == bond::FAST_PROTOCOL
     || word == bond::COMPACT_PROTOCOL
     || word == bond::SIMPLE_PROTOCOL)
        return marshal;

    if (TryProtocol(mbp))
        return fast;

    if (TryProtocol(cbp))
        return compact;

    if (TryProtocol(cbp2))
        return compact2;

    return simple;
}


struct UnknownSchema;

bond::SchemaDef LoadSchema(const std::string& file)
{
    InputFile input(file), tryJson(input);

    char c;
    tryJson.Read(c);
        
    return (c == '{')
        ? bond::Deserialize<bond::SchemaDef>(bond::SimpleJsonReader<InputFile>(input))
        : bond::Unmarshal<bond::SchemaDef>(input);
}

template <typename Reader, typename Writer>
void TranscodeFromTo(Reader& reader, Writer& writer, const Options& options)
{
    if (!options.schema.empty())
    {
        bond::SchemaDef schema(LoadSchema(options.schema));
        bond::bonded<void, typename bond::ProtocolReader<typename Reader::Buffer> >(reader, bond::RuntimeSchema(schema)).Serialize(writer);
    }
    else
    {
        bond::bonded<UnknownSchema, typename bond::ProtocolReader<typename Reader::Buffer> >(reader).Serialize(writer);
    }
}


template <typename Writer>
void TranscodeFromTo(InputFile& input, Writer& writer, const Options& options)
{
    if (!options.schema.empty())
    {
        bond::SchemaDef schema(LoadSchema(options.schema));
        bond::SelectProtocolAndApply(bond::RuntimeSchema(schema), input, SerializeTo(writer));
    }
    else
    {
        bond::SelectProtocolAndApply<UnknownSchema>(input, SerializeTo(writer));
    }
}


template <typename Reader>
bool TranscodeFrom(Reader reader, const Options& options)
{
    FILE* file;

    if (options.output == "stdout")
        file = stdout;
    else
        file = fopen(options.output.c_str(), "wb");

    bond::StdioOutputStream out(file);

    switch (options.to)
    {
        case compact:
        {
            bond::CompactBinaryWriter<bond::StdioOutputStream> writer(out);
            TranscodeFromTo(reader, writer, options);
            return true;
        }
        case compact2:
        {
            bond::CompactBinaryWriter<bond::StdioOutputStream> writer(out, bond::v2);
            TranscodeFromTo(reader, writer, options);
            return true;
        }
        case fast:
        {
            bond::FastBinaryWriter<bond::StdioOutputStream> writer(out);
            TranscodeFromTo(reader, writer, options);
            return true;
        }
        case simple:
        {
            bond::SimpleBinaryWriter<bond::StdioOutputStream> writer(out);
            TranscodeFromTo(reader, writer, options);
            return true;
        }
        case simple2:
        {
            bond::SimpleBinaryWriter<bond::StdioOutputStream> writer(out, bond::v2);
            TranscodeFromTo(reader, writer, options);
            return true;
        }
        case json:
        {
            bond::SimpleJsonWriter<bond::StdioOutputStream> writer(out, true, 4, options.all_fields);
            TranscodeFromTo(reader, writer, options);
            return true;
        }
        default:
            return false;
    }
}


bool Transcode(InputFile& input, const Options& options)
{
    switch (options.from)
    {
        case marshal:
            return TranscodeFrom(input, options);
        case compact:
            return TranscodeFrom(bond::CompactBinaryReader<InputFile>(input), options);
        case compact2:
            return TranscodeFrom(bond::CompactBinaryReader<InputFile>(input, bond::v2), options);
        case fast:
            return TranscodeFrom(bond::FastBinaryReader<InputFile>(input), options);
        case simple:
            return TranscodeFrom(bond::SimpleBinaryReader<InputFile>(input), options);
        case simple2:
            return TranscodeFrom(bond::SimpleBinaryReader<InputFile>(input, bond::v2), options);
        default:
            return false;
    }
}


int main(int argc, char** argv)
{
    try
    {
        bf::Options options = bond::cmd::GetArgs<bf::Options>(argc, argv);

        if (!options.help)
        {
            InputFile input(options.file);

            if (options.from == guess)
                std::cerr << "Guessed " << ToString(options.from = Guess(input)) << std::endl;

            if (Transcode(input, options))
                return 0;
        }
    }
    catch(const std::exception& error)
    {
        std::cerr << std::endl << error.what() << std::endl;
    }

    bond::cmd::ShowUsage<bf::Options>(argv[0]);

    return 1;
}

