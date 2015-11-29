#pragma once

#include <fstream>
#include <bond/core/exception.h>
#include <bond/core/blob.h>

class InputFile
{
public:
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
            BOND_THROW(bond::StreamException, "Error " << std::strerror(errno) << " opening file " << name);
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
            BOND_THROW(bond::StreamException, "Error " << std::strerror(errno) << " opening file " << that.name);
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
    
private:
    mutable std::ifstream file;
    std::string name;
};

