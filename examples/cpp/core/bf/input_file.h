#pragma once

#include "err.h"
#include <bond/core/customize.h>
#include <bond/core/blob.h>
#include <bond/core/exception.h>
#include <bond/stream/input_buffer.h>
#include <boost/mpl/list.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/joint_view.hpp>
#include <vector>
#include <fstream>
#include <string>
#include <cassert>


class InputFile
{
public:
#if defined(_MSC_VER) && _MSC_VER < 1900
    using range_type = bond::blob;
#endif

    InputFile()
    {}

    InputFile(const std::string& name)
        : file(name, std::ios::binary),
          name(name)
    {
        if (file.good())
        {
            file.exceptions(std::ifstream::failbit | std::ifstream::badbit | std::ifstream::eofbit);
        }
        else
        {
            BOND_THROW(bond::StreamException, "Error " << ErrorString(errno) << " opening file " << name);
        }
    }

    // Copy ctor opens a new stream in order to keep independent file pointer.
    InputFile(const InputFile& that)
        : file(that.name, std::ios::binary),
          name(that.name)
    {
        if (file.good())
        {
            file.exceptions(std::ifstream::failbit | std::ifstream::badbit | std::ifstream::eofbit);
            file.seekg(that.file.tellg());
        }
        else
        {
            BOND_THROW(bond::StreamException, "Error " << ErrorString(errno) << " opening file " << that.name);
        }
    }

    InputFile& operator=(const InputFile&);

    bool operator==(const InputFile& that) const
    {
        return this == &that;
    }

    template <typename T>
    void Read(T& value)
    {
        Read(&value, sizeof(T));
    }

    void Read(void *buffer, uint32_t size)
    {
        file.read(static_cast<char*>(buffer), size);
    }

    void Read(bond::blob& blob, uint32_t size)
    {
        boost::shared_ptr<char[]> buffer = boost::make_shared_noinit<char[]>(size);

        Read(buffer.get(), size);
        blob.assign(buffer, 0, size);
    }

    void Skip(uint32_t size)
    {
        file.seekg(size, std::ios::cur);
    }

    friend std::pair<InputFile, std::ifstream::pos_type> GetCurrentBuffer(const InputFile& input)
    {
        return std::make_pair(input, input.file.tellg());
    }

    friend bond::InputBuffer CreateInputBuffer(const InputFile& /*other*/, const bond::blob& blob)
    {
        return bond::InputBuffer(blob);
    }

private:
    mutable std::ifstream file;
    std::string name;
};


inline bond::blob GetBufferRange(
    std::pair<InputFile, std::ifstream::pos_type> begin,
    const std::pair<InputFile, std::ifstream::pos_type>& end)
{
    assert(begin.second <= end.second);

    bond::blob blob;
    begin.first.Read(blob, static_cast<uint32_t>(end.second - begin.second));
    return blob;
}


template <typename InputBuffer>
struct rebind_input_buffer
{
    template <typename T> struct
    transform
    {
        typedef T type;
    };

    template <template <typename> class Reader, typename Buffer> struct
    transform<Reader<Buffer>>
    {
        typedef Reader<InputBuffer> type;
    };
};


namespace bond
{
    template <> struct
    customize<protocols>
    {
        template <typename T> struct
        modify
        {
            typedef typename boost::mpl::transform_view<
                T,
                typename rebind_input_buffer<InputFile>::template transform<boost::mpl::_>
            >::type protocols_with_inputfile;

            typedef typename boost::mpl::transform_view<
                T,
                typename rebind_input_buffer<InputFile&>::template transform<boost::mpl::_>
            >::type protocols_with_inputfile_ref;

            typedef typename boost::mpl::joint_view<T, protocols_with_inputfile>::type type1;

            typedef typename boost::mpl::joint_view<type1, protocols_with_inputfile_ref>::type type;
        };
    };

} // namespace bond