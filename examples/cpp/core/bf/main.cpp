#include "cmd_arg_reflection.h"
#include "err.h"
#include "input_file.h"
#include <bond/core/cmdargs.h>
#include <bond/protocol/simple_json_writer.h>
#include <bond/stream/stdio_output_stream.h>
#include <errno.h>
#include <iostream>
#include <stdio.h>

using namespace bf;

FILE* OpenFile(const char* path, const char* mode)
{
    FILE* file;

#ifdef _MSC_VER

    // Under the compiler settings we use with MSVC, fopen is not considered
    // "secure", so we use the "secure" variant.
    errno_t err = fopen_s(&file, path, mode);
    if (err != 0)
    {
        BOND_THROW(bond::StreamException, "Error " << ErrorString(err) << " opening file " << path);
    }

#else

    file = fopen(path, mode);
    if (file == nullptr)
    {
        BOND_THROW(bond::StreamException, "Error " << ErrorString(errno) << " opening file " << path);
    }

#endif

    return file;
}

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


// Here using a bond::InputBuffer for marshaled bonded protocols
// instead of defaulted one because corresponding CreateInputBuffer
// returns bond::InputBuffer instead of InputFile.
using MarshaledBondedProtocols = bond::Protocols<bond::CompactBinaryReader<bond::InputBuffer> >;

using NewProtocols = bond::BuiltInProtocols::Append<
    bond::CompactBinaryReader<InputFile>,
    bond::FastBinaryReader<InputFile>,
    bond::SimpleBinaryReader<InputFile, MarshaledBondedProtocols>,
    bond::SimpleJsonReader<InputFile>,
    bond::CompactBinaryReader<InputFile&>,
    bond::FastBinaryReader<InputFile&>,
    bond::SimpleBinaryReader<InputFile&, MarshaledBondedProtocols>,
    bond::SimpleJsonReader<InputFile&> >;

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
        ? bond::Deserialize<bond::SchemaDef, NewProtocols>(bond::SimpleJsonReader<InputFile>(input))
        : bond::Unmarshal<bond::SchemaDef, NewProtocols>(input);
}

template <typename Reader, typename Writer>
void TranscodeFromTo(Reader& reader, Writer& writer, const Options& options)
{
    if (!options.schema.empty() && !options.schema.front().empty())
    {
        bond::SchemaDef schema(LoadSchema(options.schema.front()));
        bond::bonded<void, bond::ProtocolReader>(reader, bond::RuntimeSchema(schema)).template Serialize<NewProtocols>(writer);
    }
    else
    {
        bond::bonded<UnknownSchema, bond::ProtocolReader>(reader).template Serialize<NewProtocols>(writer);
    }
}


template <typename Writer>
void TranscodeFromTo(InputFile& input, Writer& writer, const Options& options)
{
    if (!options.schema.empty() && !options.schema.front().empty())
    {
        bond::SchemaDef schema(LoadSchema(options.schema.front()));
        bond::SelectProtocolAndApply<NewProtocols>(bond::RuntimeSchema(schema), input, bond::SerializeTo<NewProtocols>(writer));
    }
    else
    {
        bond::SelectProtocolAndApply<UnknownSchema, NewProtocols>(input, bond::SerializeTo<NewProtocols>(writer));
    }
}


template <typename Reader>
bool TranscodeFrom(Reader reader, const Options& options)
{
    FILE* file;

    if (options.output == "stdout")
    {
        file = stdout;
    }
    else
    {
        file = OpenFile(options.output.c_str(), "wb");
    }

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

template <typename Input>
bool Transcode(Input input, const Options& options)
{
    bf::Protocol from = options.from.empty() ? guess : options.from.front();

    if (from == guess)
    {
        from = Guess(input);
        std::cerr << std::endl << "Guessed " << ToString(from) << std::endl;
    }

    switch (from)
    {
        case marshal:
            return TranscodeFrom(input, options);
        case compact:
            return TranscodeFrom(bond::CompactBinaryReader<Input>(input), options);
        case compact2:
            return TranscodeFrom(bond::CompactBinaryReader<Input>(input, bond::v2), options);
        case fast:
            return TranscodeFrom(bond::FastBinaryReader<Input>(input), options);
        case simple:
            return TranscodeFrom(bond::SimpleBinaryReader<Input>(input), options);
        case simple2:
            return TranscodeFrom(bond::SimpleBinaryReader<Input>(input, bond::v2), options);
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

            do
            {
                // In order to decode multiple payloads from a file we need to
                // use InputFile& however that usage doesn't support marshalled
                // bonded<T> in untagged protocols. As a compromise we use
                // InputFile for the last payload and InputFile& otherwise.
                if (options.schema.size() > 1 || options.from.size() > 1)
                {
                    if (!Transcode<InputFile&>(input, options))
                        return 1;
                }
                else
                {
                    if (!Transcode<InputFile>(input, options))
                        return 1;
                }

                if (!options.schema.empty())
                    options.schema.pop_front();

                if (!options.from.empty())
                    options.from.pop_front();
            }
            while (!options.schema.empty() || !options.from.empty());

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
