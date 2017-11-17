// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/tokenizer.hpp>

#include <iostream>

namespace bond
{
namespace cmd
{
namespace detail
{

// Enum types already have generated overload for ToString.
// For other types we use boost::lexical_cast.
template <typename T>
std::string ToString(const T& value)
{
    return boost::lexical_cast<std::string>(value);
}

// lexical_cast treats uint8_t and int8_t as characters, not numbers
std::string ToString(const uint8_t& value)
{
    return boost::lexical_cast<std::string>(static_cast<int>(value));
}

std::string ToString(const int8_t& value)
{
    return boost::lexical_cast<std::string>(static_cast<int>(value));
}

class Metadata : boost::noncopyable
{
public:
    Metadata(const bond::Metadata& metadata)
        : metadata(metadata)
    {}

    std::string Flag() const
    {
        std::string flag(metadata.name);
        std::replace(flag.begin(), flag.end(), '_', '-');
        return "--" + flag;
    }

    std::string Param() const
    {
        if (IsNaked())
            return boost::to_upper_copy(metadata.name);
        else
            return Flag() + "=" + boost::to_upper_copy(metadata.name);
    }

    std::string Abbr() const
    {
        if (HasAttribute("abbr"))
            return "-" + Attribute("abbr");
        else
            return "";
    }

    template <typename T>
    std::string Help() const
    {
        std::string help;

        if (HasAttribute("help"))
            help = Attribute("help");
        else
            help = HelpFromType<T>();

        std::string default_value = Default<T>();

        if (!default_value.empty())
        {
            if (!help.empty())
                help += ", ";

            help += "default " + default_value;
        }

        return help;
    }

    std::string Help() const
    {
        return Attribute("help");
    }

    bool IsNaked() const
    {
        return HasAttribute("naked");
    }

    bool IsOptional() const
    {
        return metadata.modifier == bond::Optional;
    }

private:
    bool HasAttribute(const std::string& attr) const
    {
        return metadata.attributes.end() != metadata.attributes.find(attr);
    }

    std::string Attribute(const std::string& attr) const
    {
        std::map<std::string, std::string>::const_iterator it;

        it = metadata.attributes.find(attr);

        if (it != metadata.attributes.end())
            return it->second;
        else
            return "";
    }

    template <typename T>
    typename boost::disable_if_c<bond::is_list_container<T>::value
        || std::is_enum<T>::value, std::string>::type
    HelpFromType() const
    {
            return "";
    }

    template <typename T>
    typename boost::enable_if<std::is_enum<T>, std::string>::type
    HelpFromType() const
    {
            std::string enums;

            const std::map<std::string, T>& names = bond::GetEnumNames<T>();

            for (typename std::map<std::string, T>::const_iterator it = names.begin(); it != names.end(); ++it)
            {
                if (!enums.empty())
                    enums += " | ";
                enums += it->first;
            }
            return enums;
    }

    template <typename T>
    typename boost::enable_if<bond::is_list_container<T>, std::string>::type
    HelpFromType() const
    {
            std::string help = HelpFromType<typename bond::element_type<T>::type>();

            if (!help.empty())
                help = "comma-separated list of: " + help;

            return help;
    }

    template <typename T>
    typename boost::disable_if<bond::is_basic_type<T>, std::string>::type
    Default() const
    {
            return "";
    }

    template <typename T>
    typename boost::enable_if<bond::is_basic_type<T>, std::string>::type
    Default() const
    {
            if (metadata.modifier == bond::Optional && !metadata.default_value.nothing)
            {
                T var;

                bond::detail::VariantGet(metadata.default_value, var);

                return ToString(var);
            }
            else
                return "";
    }

    const bond::Metadata& metadata;
};


// Options
class Options
{
public:
    // ctor
    Options(int argc, char** argv)
    {
        for (int i = 1; i < argc; ++i)
        {
            std::string value = argv[i];
            std::string name;

            if (value[0] == '-' && value != "-" && value != "--")
            {
                size_t pos = value.find_first_of("=:");

                name = value.substr(0, pos);

                // unpack multiple abbreviated options in single param, e.g.:
                // -fxd -> -f -x -d
                if (name[1] != '-')
                    while(name.length() > 2)
                    {
                        params.push_back(Param(name.substr(0, 2), ""));
                        name.erase(1, 1);
                    }

                if (pos != std::string::npos)
                    value = value.substr(pos + 1);
                else
                    value = "";
            }

            params.push_back(Param(name, value));
        }
    }

    // GetFlag
    bool GetFlag(const std::string& flag, const std::string& abbr)
    {
        Params::iterator it = FindParam(flag, abbr);

        if (it != params.end())
        {
            if (!it->value.empty())
                throw std::runtime_error("Invalid parameter(s):\n    " + it->value);

            return true;
        }

        return false;
    }

    // GetParam
    bool GetParam(std::string& value)
    {
        return GetParam("", "", value);
    }

    // GetParam
    bool GetParam(const std::string& flag, const std::string& abbr, std::string& value)
    {
        Params::iterator it = FindParam(flag, abbr);

        if (it != params.end())
        {
            value = it->value;

            if(value.empty() && ++it != params.end() && !it->used && it->name.empty())
            {
                value = it->value;
                it->used = true;
            }

            return true;
        }

        return false;
    }

    // Leftovers
    std::string Leftovers()
    {
        std::string leftovers;

        for (Params::iterator it = params.begin(); it != params.end(); ++it)
            if (!it->used)
            {
                leftovers += "    " + it->name;

                if (!it->name.empty() && !it->value.empty())
                    leftovers += "=";

                leftovers += it->value + "\n";
            }

        return leftovers;
    }

private:
    struct Param
    {
        Param(const std::string& name, const std::string& value)
            : name(name),
                value(value),
                used(false)
        {}

        std::string name;
        std::string value;
        bool        used;
    };

    typedef std::list<Param> Params;

    Params::iterator FindParam(const std::string& flag, const std::string& abbr)
    {
        Params::iterator it;

        for (it = params.begin(); it != params.end(); ++it)
            if (!it->used && (it->name == flag || (it->name == abbr && !abbr.empty())))
            {
                it->used = true;
                break;
            }

        return it;
    }

    Params params;
};


// CmdArg
class CmdArg
    : public bond::ModifyingTransform
{
public:
    CmdArg(int argc, char** argv, bool partial = false)
        : options(boost::make_shared<Options>(argc, argv)),
          partial(partial)
    {}

    CmdArg(const boost::shared_ptr<Options>& options, bool partial = false)
        : options(options),
          partial(partial)
    {}

    void Begin(const detail::Metadata& /*metadata*/) const
    {}

    void End() const
    {
        if (!partial)
        {
            std::string leftovers = options->Leftovers();

            if (!leftovers.empty())
                throw std::runtime_error("Invalid parameter(s):\n" + leftovers);
        }
    }

    template <typename T>
    bool Base(T& value) const
    {
        return bond::Apply(CmdArg(options, true), value);
    }

    template <typename T>
    typename boost::enable_if<bond::is_list_container<T>, bool>::type
    Field(uint16_t /*id*/, const detail::Metadata& metadata, T& var) const
    {
        while(GetParam(metadata, var));
        return false;
    }

    template <typename T>
    typename boost::disable_if<bond::is_list_container<T>, bool>::type
    Field(uint16_t /*id*/, const detail::Metadata& metadata, T& var) const
    {
        GetParam(metadata, var);
        return false;
    }

private:
    // Parse enum
    template <typename T>
    typename boost::enable_if<std::is_enum<T> >::type
    Parse(const std::string& value, T& var) const
    {
        if (!ToEnum(var, value.c_str()))
            throw std::runtime_error("Invalid parameter(s):\n    " + value);
    }

    // Parse number
    template <typename T>
    typename boost::enable_if<std::is_arithmetic<T> >::type
    Parse(const std::string& value, T& var) const
    {
        try
        {
            var = boost::lexical_cast<T>(value);
        }
        catch(const std::exception&)
        {
            throw std::runtime_error("Invalid parameter(s):\n    " + value);
        }
    }

    // Parse string
    void Parse(const std::string& value, std::string& var) const
    {
        var = value;
    }

    // Parse list
    template <typename T>
    typename boost::enable_if<bond::is_list_container<T> >::type
    Parse(const std::string& value, T& var) const
    {
        typedef boost::tokenizer<boost::escaped_list_separator<char> > tokenizer;

        tokenizer tok(value);

        for(tokenizer::iterator it = tok.begin(); it != tok.end(); ++it)
        {
            typename bond::element_type<T>::type tmp;

            Parse(*it, tmp);
            var.push_back(tmp);
        }
    }

    template <typename T>
    void Parse(const std::string& value, maybe<T>& var) const
    {
        Parse(value, var.set_value());
    }


    // bool param
    bool GetParam(const detail::Metadata& metadata, bool& var) const
    {
        // required or naked bool flags don't make sense
        BOOST_ASSERT(metadata.IsOptional());
        BOOST_ASSERT(!metadata.IsNaked());

        return (var = options->GetFlag(metadata.Flag(), metadata.Abbr()));
    }


    // generic param
    template <typename T>
    bool GetParam(const detail::Metadata& metadata, T& var) const
    {
        std::string value;

        if (metadata.IsNaked())
            options->GetParam(value);
        else
            options->GetParam(metadata.Flag(), metadata.Abbr(), value);

        if (value.empty())
        {
            if(!metadata.IsOptional())
                throw std::runtime_error("Required parameter " + metadata.Param() + " missing.");
            else
                return false;
        }

        Parse(value, var);

        return true;
    }

    boost::shared_ptr<Options> options;
    bool partial;
};


class Usage
    : public bond::SerializingTransform
{
public:
    Usage(const char* program, std::ostream& out = std::cerr)
        : program(program),
          out(out)
    {}

    void Begin(const detail::Metadata& metadata) const
    {
        std::string help = metadata.Help();

        if (!help.empty())
            out << std::endl << "Usage: " << program << " " << help << std::endl << std::endl;
    }

    void End() const
    {}

    template <typename T>
    bool Base(const T& value) const
    {
        bond::Apply(*this, value);
        return false;
    }

    bool Field(uint16_t /*id*/, const detail::Metadata& metadata, const bool& /*value*/) const
    {
        Print(metadata.Flag(), metadata.Abbr(), metadata.Help());
        return false;
    }

    template <typename T>
    bool Field(uint16_t /*id*/, const detail::Metadata& metadata, const bond::maybe<T>& /*value*/) const
    {
        std::string help = metadata.Help<T>();

        Print(metadata.Param(), metadata.Abbr(), help);
        return false;
    }

    template <typename T>
    bool Field(uint16_t /*id*/, const detail::Metadata& metadata, const T& /*value*/) const
    {
        std::string help = metadata.Help<T>();

        Print(metadata.Param(), metadata.Abbr(), help);
        return false;
    }

protected:
    void Print(const std::string& arg, const std::string& abbr, const std::string& help) const
    {
        int indent = (std::max)(30, static_cast<int>(arg.size()) + 5);
        std::string formated = FormatHelp(help, indent);
        out << " ";
        out.width(2);
        out << abbr << " ";
        out.setf(std::ios::left, std::ios::adjustfield);
        out.width(indent - 4);
        out << arg << formated << std::endl;
    }

    std::string FormatHelp(const std::string& help, int indent) const
    {
        std::string formated = help;
        int i = 79 - indent;

        for (int begin = 0; i < static_cast<int>(formated.length()); begin += 80, i = begin + 79 - indent)
        {
            while (i >= begin && !isspace(static_cast<int>(formated[i])))
                --i;

            while (i < static_cast<int>(formated.length()) && isspace(static_cast<int>(formated[i])))
                ++i;

            formated.insert(i, begin + 80 - i, ' ');
            formated[i + begin + 79 - i - indent] = '\n';
        }

        return formated;
    }

    std::string program;
    std::ostream& out;
};

} // namespace detail
} // namespace cmd
} // namespace bond
