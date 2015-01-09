#include "output_stream_allocator_reflection.h"
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::output_stream_allocator;

// This example shows the use of a custom allocator around a fixed buffer with
// OutputMemoryStream. OutputMemoryStream<T> can be used with any STL style
// allocator.
// OutputBuffer is just a type alias for OutputMemoryStream where the allocator
// type defaults to std::allocator<char>.

// Simple allocator using a fixed size buffer 
template<typename T = char>
class CustomAllocator
{
public:
    typedef typename boost::remove_const<T>::type value_type;
    typedef value_type* pointer;
    typedef value_type& reference;
    typedef const value_type* const_pointer;
    typedef const value_type& const_reference;
    typedef size_t size_type;
    typedef ptrdiff_t difference_type;

    template<size_t N> 
    CustomAllocator(char (&buffer)[N])
        : buffer(buffer), 
          total(N),
          index(*static_cast<size_t*>(static_cast<void*>(buffer)))
    {
        index = sizeof(index);
    }
    
    template<typename U>
    CustomAllocator(const CustomAllocator<U>& that)
        : buffer(that.buffer), 
          total(that.total),
          index(that.index)
    {}

    template<class Other>
    struct rebind
    {
        // convert this type to CustomAllocator<Other>
        typedef CustomAllocator<Other> other;
    };

    bool operator==(const CustomAllocator& that) const
    {
        return (this == &that);
    }

    bool operator!=(const CustomAllocator& that) const
    {
        return (this != &that);
    }

    pointer address(reference val) const
    {
        return &val;
    }

    const_pointer address(const_reference val) const
    {
        return &val;
    }

    void deallocate(pointer, size_type)
    {
    }

    pointer allocate(size_type count)
    {
        size_type size = count * sizeof(T);
        if (index + size <= total)
        {
            // save the start of the block
            void* result = &buffer[index];
            // advance index to next block
            index += size;
            // return address to the block
            return static_cast<T*>(result);
        } 
        else 
        {
            // no room in the buffer
            throw std::bad_alloc();
        }
    }

    pointer allocate(size_type count, const void*)
    {
        // allocate array of count elements, ignore hint
        return allocate(count);
    }

    void construct(pointer ptr, const T& val)
    {
        // construct object at ptr with value val
        new((void*)ptr) T(val);
    }

#if !defined(BOND_NO_CXX11_ALLOCATOR)
    template <typename U, typename ...Args>
    void construct(U* ptr, Args&&...args)
    {
        new ((void*)ptr) (U)(std::forward<Args>(args)...);
    }
#endif

    void destroy(pointer ptr)
    {
        (void)ptr;
        // destroy object at ptr
        ptr->~T();
    }

    size_type max_size() const
    {
        return (total - index) / sizeof(T); 
    }

private:
    template <typename U>
    friend class CustomAllocator;

    char* buffer;
    const size_t total;
    size_t& index;
};


// output memory stream supports any STL style of custom allocators
typedef bond::OutputMemoryStream<CustomAllocator<> > CustomStream;

bond::blob Serialize(const Struct& obj, CustomAllocator<>& allocator) 
{
    // initialize output stream with your custom allocator
    CustomStream buffer(allocator);
    // use your custom allocator with your protocol writer
    bond::CompactBinaryWriter<CustomStream> writer(buffer);
    bond::Serialize(obj, writer);
    return buffer.GetBuffer();
}

void Deserialize(const bond::blob& buffer, Struct& obj) 
{
    bond::CompactBinaryReader<bond::InputBuffer> reader(buffer);    
    bond::Deserialize(reader, obj);
}

int main() 
{
    Struct obj;
    obj.items.push_back(1.1);
    obj.items.push_back(0.9);    
    obj.num = 0x1000;
    obj.str = "test";

    // initialize with fixed buffer
    char buffer[8192];
    CustomAllocator<> allocator(buffer);
    
    // serialize using our allocator
    bond::blob blob;
    blob = Serialize(obj, allocator);

    // note: blob points to our buffer
    assert(blob.data() >= buffer);
    assert(blob.data() < buffer + sizeof(buffer));
    assert(blob.size() < sizeof(buffer));

    // deserialize from our buffer
    Struct obj2;
    Deserialize(blob, obj2);

    return 0;    
}
