// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "reflection.h"
#include "exception.h"
#include "null.h"
#include "detail/tags.h"
#include "detail/odr.h"
#include "detail/omit_default.h"
#include "detail/debug.h"
#include "detail/double_pass.h"
#include "detail/marshaled_bonded.h"

#include <boost/static_assert.hpp>

namespace bond
{

// Transforms take input from parser as a series of calls to the Field methods.
// Arguments for the calls privide id, name and value of the fields. Return
// value from Field methods must be convertible to bool, with the value of true
// indicating that the transform has completed and the parser may exit.


//
// Serializer writes input using provided protocol writer. 
// When the input is comming from parsing a struct, applying this transform is  
// equivalent to serialization using the specfied protocol. 
// Applying this transform to input from parsing serialized data is equivalent  
// to transcoding from one protocol to another.
//
template <typename Writer>
class Serializer
    : public SerializingTransform
{
public:
    typedef Writer writer_type;

    BOOST_STATIC_ASSERT(is_writer<Writer>::value);

    Serializer(Writer& output, bool base = false)
        : _output(output),
          _base(base)
    {}

    
    bool NeedPass0() const
    {
        return _output.NeedPass0();
    }

    template <typename Pass0>
    Serializer<Pass0> Rebind(Pass0& pass0) const
    {
        return Serializer<Pass0>(pass0);
    }

    void Begin(const Metadata& metadata) const
    {
        _output.WriteStructBegin(metadata, _base);
    }

    void End() const
    {
        _output.WriteStructEnd(_base);
    }

    void UnknownEnd() const
    {
        _output.WriteStructEnd(true);
    }

    template <typename T>
    bool Base(const T& value) const
    {
        // 'true' means that we are writing a base struct 
        Apply(Serializer<Writer>(_output, true), value);
        return false;
    }

    template <typename T>
    bool Field(uint16_t id, const Metadata& metadata, const T& value) const
    {
        if (detail::omit_field<Writer>(metadata, value))
        {
            detail::WriteFieldOmitted(_output, GetTypeId(value), id, metadata);
            return false;
        }

        WriteField(id, metadata, value);
        return false;
    }

    template <typename T>
    bool Field(uint16_t id, const Metadata& metadata, const maybe<T>& value) const
    {
        if (detail::omit_field<Writer>(metadata, value))
        {
            detail::WriteFieldOmitted(_output, get_type_id<T>::value, id, metadata);
            return false;
        }

        WriteField(id, metadata, value.value());
        return false;
    }
    
    // unknown field
    template <typename T>
    bool UnknownField(uint16_t id, const T& value) const
    {
        _output.WriteFieldBegin(GetTypeId(value), id);
        Write(value);
        _output.WriteFieldEnd();
        return false;
    }


    // omitted field
    bool OmittedField(uint16_t id, const Metadata& metadata, BondDataType type) const
    {
        detail::WriteFieldOmitted(_output, type, id, metadata);
        return false;
    }

    
    template <typename T>
    void Container(const T& element, uint32_t size) const
    {
        _output.WriteContainerBegin(size, GetTypeId(element));

        while (size--)
            Write(element);
    
        _output.WriteContainerEnd();
    }


    template <typename Key, typename T>
    void Container(const Key& key, const T& value, uint32_t size) const
    {
        _output.WriteContainerBegin(size, std::make_pair(GetTypeId(key), GetTypeId(value)));

        while (size--)
        {
            Write(key);
            Write(value);
        }
    
        _output.WriteContainerEnd();
    }

private:
    // basic type field
    template <typename T>
    typename boost::enable_if_c<is_basic_type<T>::value && !is_type_alias<T>::value && true>::type
    WriteField(uint16_t id, const Metadata& metadata, const T& value) const
    {
        _output.WriteField(id, metadata, value);
    }

    // struct or container field
    template <typename T>
    typename boost::disable_if_c<is_basic_type<T>::value && !is_type_alias<T>::value>::type
    WriteField(uint16_t id, const Metadata& metadata, const T& value) const
    {
        _output.WriteFieldBegin(GetTypeId(value), id, metadata);
        Write(value);
        _output.WriteFieldEnd();
    }

    // basic type value
    template <typename T>
    typename boost::enable_if_c<is_basic_type<T>::value && !is_type_alias<T>::value && true>::type
    Write(const T& value) const
    {
        _output.Write(value);
    }

    // type alias
    template <typename T>
    typename boost::enable_if<is_type_alias<T> >::type
    Write(const T& value) const
    {
        Write(get_aliased_value(value));
    }

    // struct value or bonded<T>
    template <typename T>
    typename boost::enable_if<is_bond_type<T> >::type
    Write(const T& value) const
    {
        Apply(Serializer<Writer>(_output), value);
    }

    // bonded<T> and untagged writer
    template <typename T>
    typename boost::enable_if<uses_marshaled_bonded<typename Writer::Reader, T> >::type
    Write(const bonded<T>& value) const
    {
        detail::MarshalToBlob(value, _output);
    }

    // bonded<void> and untagged writer
    template <typename Reader>
    typename boost::enable_if<uses_marshaled_bonded<typename Writer::Reader, Reader> >::type
    Write(const bonded<void, Reader>& value) const
    {
        value.Serialize(_output);
    }

    // 2-tuple
    template <typename T1, typename T2>
    void Write(const std::pair<T1, T2>& value) const
    {
        Write(value.first);
        Write(value.second);
    }

    // container value
    template <typename T>
    typename boost::enable_if<is_container<T> >::type
    Write(const T& value) const
    {
        _output.WriteContainerBegin(container_size(value), get_type_id<typename element_type<T>::type>::value);

        for (const_enumerator<T> items(value); items.more();)
        {
            Write(items.next());
        }

        _output.WriteContainerEnd();
    }


    // blob
    void Write(const blob& value) const
    {
        _output.WriteContainerBegin(value.length(), get_type_id<blob::value_type>::value);
        _output.Write(value);
        _output.WriteContainerEnd();
    }


    // serialized value
    template <typename Reader, typename T>
    typename boost::enable_if<is_basic_type<T> >::type
    Write(const value<T, Reader>& value) const
    {
        T data;
        
        value.Deserialize(data);
        Write(data);
    }
    
    template <typename Reader, typename T>
    typename boost::disable_if<is_basic_type<T> >::type
    Write(const value<T, Reader>& value) const
    {
        Apply(Serializer<Writer>(_output), value);
    }

    
    template <typename T, typename WriterT>
    friend class Merger;

    template <typename T, typename Reader, typename Enable>
    friend class value;

    template <typename T, typename Schema, typename Transform>
    friend class detail::_Parser;

    template <typename Transform, typename T>
    friend bool detail::DoublePassApply(const Transform&, const T&);

protected:
    Writer&     _output;
    const bool  _base;
};


// SerializeTo
template <typename Writer>
Serializer<Writer> SerializeTo(Writer& output)
{
    return Serializer<Writer>(output);
}


template <typename Writer>
class Marshaler
    : protected Serializer<Writer>
{
public:
    typedef Writer writer_type;

    Marshaler(Writer& output)
        : Serializer<Writer>(output)
    {}
    
    template <typename T>
    bool Marshal(const T& value) const
    {
        this->_output.WriteVersion();
        return Apply(static_cast<const Serializer<Writer>&>(*this), value);
    }
};


template <typename Writer, typename T, typename Reader>
bool inline
Apply(const Marshaler<Writer>& marshaler, const bonded<T, Reader>& bonded)
{
    return marshaler.Marshal(bonded);
}


template <typename Writer, typename T>
bool inline 
Apply(const Marshaler<Writer>& marshaler, const T& value)
{
    return marshaler.Marshal(value);
}


// MarshalTo
template <typename Writer>
Marshaler<Writer> MarshalTo(Writer& output)
{
    return Marshaler<Writer>(output);
}


template <typename T>
class RequiredFieldValiadator
{
protected:
    void Begin() const
    {
        _required = next_required_field<typename schema<T>::type::fields>::value;
    }
        
    template <typename Head>
    typename boost::enable_if<is_same<typename Head::field_modifier, 
                                      reflection::required_field_modifier> >::type
    Validate() const
    {
        if (_required == Head::id)
            _required = next_required_field<typename schema<T>::type::fields, Head::id + 1>::value;
        else
            MissingFieldException();
    }


    template <typename Schema>
    typename boost::enable_if_c<next_required_field<typename Schema::fields>::value 
                             != invalid_field_id>::type
    Validate() const
    {
        if (_required != invalid_field_id)
            MissingFieldException();
    }


    template <typename Head>
    typename boost::disable_if<is_same<typename Head::field_modifier, 
                                       reflection::required_field_modifier> >::type
    Validate() const
    {}


    template <typename Schema>
    typename boost::disable_if_c<next_required_field<typename Schema::fields>::value 
                              != invalid_field_id>::type
    Validate() const
    {}

private:
    void MissingFieldException() const;
    
    mutable uint16_t _required;
};

template <typename T>
void RequiredFieldValiadator<T>::MissingFieldException() const
{
    BOND_THROW(CoreException,
          "De-serialization failed: required field " << _required <<
          " is missing from " << schema<T>::type::metadata.qualified_name);
}

//
// To<T> transforms the input field-by-field, matching both field ids and types,
// into an instance of a static bond type T.
//
namespace detail
{

class To
    : public DeserializingTransform
{
public:
    void UnknownEnd() const
    {}

    bool UnknownField(...) const
    {
        return false;
    }

protected:
    template <typename V, typename X>
    void AssignToVar(V& var, const X& value) const
    {
        value.Deserialize(var);
    }

    template <typename V, typename X>
    void AssignToVar(maybe<V>& var, const X& value) const
    {
        value.Deserialize(var.set_value());
    }

    template <typename V, typename X>
    typename boost::enable_if<has_base<V>, bool>::type
    AssignToBase(V& var, const X& value) const
    {
        return Apply(bond::To<typename schema<V>::type::base>(var), value);
    }

    template <typename V, typename X>
    typename boost::disable_if<has_base<V>, bool>::type
    AssignToBase(V& /*var*/, const X& /*value*/) const
    {
        return false;
    }

    template <typename X>
    bool AssignToField(const boost::mpl::l_iter<boost::mpl::l_end>&, uint16_t /*id*/, const X& /*value*/) const
    {
        return false;
    }
};

} // namespace detail


template <typename T, typename Validator>
class To
    : public detail::To,
      protected Validator
{
public:
    To(T& var)
        : _var(var)
    {}

    void Begin(const Metadata& /*metadata*/) const
    {
        // Type T must be a Bond struct (i.e. struct generated by Bond codegen
        // from a .bond file). If the assert fails for a Bond struct, the likely 
        // reason is that you didn't include the generated file *_reflection.h.
        BOOST_STATIC_ASSERT(has_schema<T>::value);

        // Triggering this assert means you are reusing an object w/o resetting
        // it to default value first.
        BOOST_ASSERT(detail::OptionalDefault<T>(_var));

        Validator::Begin();
    }

    void End() const
    {
        Validator::template Validate<typename schema<T>::type>();
    }

    template <typename X>
    bool Base(const X& value) const
    {
        return AssignToBase(_var, value);
    }


    // Separate Field overloads for bonded<T>, basic types and containers allows us to use
    // simpler predicates in boost::mpl::copy_if. This doesn't matter for runtime code but
    // compiles significantly faster.
    template <typename Reader, typename X>
    bool Field(uint16_t id, const Metadata& /*metadata*/, const bonded<X, Reader>& value) const
    {
        return AssignToField(typename boost::mpl::begin<typename nested_fields<T>::type>::type(), id, value);
    }


    template <typename Reader, typename X>
    bool Field(uint16_t id, const Metadata& /*metadata*/, const value<X, Reader>& value) const
    {
        return AssignToField(typename boost::mpl::begin<typename matching_fields<T, X>::type>::type(), id, value);
    }


    template <typename Reader>
    bool Field(uint16_t id, const Metadata& /*metadata*/, const value<void, Reader>& value) const
    {
        return AssignToField(typename boost::mpl::begin<typename container_fields<T>::type>::type(), id, value);
    }


    // Fast path for the common case when parser is using compile-time schema schema<T>::type
    // and thus already knows schema type for each field.  
    typedef T FastPathType;

    template <typename FieldT, typename X>
    bool Field(const FieldT&, const X& value) const
    {
        Validator::template Validate<FieldT>();
        AssignToVar(FieldT::GetVariable(_var), value);
        return false;
    }

private:
    using detail::To::AssignToBase;
    using detail::To::AssignToField;

    template <typename Fields, typename X>
    bool AssignToField(const Fields&, uint16_t id, const X& value) const
    {
        typedef typename boost::mpl::deref<Fields>::type Head;

        if (id == Head::id)
        {
            Validator::template Validate<Head>();
            AssignToVar(Head::GetVariable(_var), value);
            return false;
        }
        else
        {
            return AssignToField(typename boost::mpl::next<Fields>::type(), id, value);
        }
    }

private:
    T& _var;
};


struct Mapping;

typedef std::vector<uint16_t> Path;
typedef std::map<uint16_t, Mapping> Mappings;

struct Mapping
{
    Path path;
    Mappings fields;
};

static const uint16_t mapping_base = invalid_field_id;

//
// MapTo<T> maps the input fields onto an instance of a static bond type T,
// using provided mappings from field path in the source to field path in
// the type T. Field paths are expressed as a lists of field ids. 
//
namespace detail
{

class MapTo
    : public DeserializingTransform
{
public:
    void Begin(const Metadata& /*metadata*/) const
    {}

    void End(bool = false) const
    {}

    void UnknownEnd() const
    {}

    template <typename T>
    bool UnknownField(uint16_t, const T&) const
    {
        return false;
    }
    
protected:
        struct PathView
        : boost::noncopyable
    {
        PathView(const Path& path)
            : path(path),
              current(path.begin())
        {}

        PathView(const Path& path, Path::const_iterator current)
            : path(path),
              current(current)
        {}

        size_t size() const
        {
            return path.end() - current;
        }

        const Path&                 path;
        const Path::const_iterator  current;
    };


    template <typename V, typename X>
    bool Assign(V& var, const PathView& ids, const X& value) const
    {
        BOOST_ASSERT(ids.size() > 0);

        if (*ids.current == mapping_base)
            return AssignToBase(base_class<typename schema<V>::type>(), var, ids, value);

        if (ids.size() == 1)
            return AssignToField(var, *ids.current, value);
        else
            return AssignToNested(var, ids, value);
    }
    
    
    template <typename V, typename X>
    bool AssignToNested(V& var, const PathView& ids, const X& value) const
    {
        return AssignToNested(typename boost::mpl::begin<typename struct_fields<V>::type>::type(), var, ids, value);
    }


    template <typename BaseT, typename V, typename X>    
    bool AssignToBase(const BaseT*, V& var, const PathView& ids, const X& value) const
    {
        return Assign(static_cast<BaseT&>(var), PathView(ids.path, ids.current + 1), value);
    }

    
    template <typename V, typename X>    
    bool AssignToBase(const no_base*, V& /*var*/, const PathView& /*ids*/, const X& /*value*/) const
    {
        return false;
    }


    template <typename Nested, typename V, typename X>
    bool AssignToNested(const Nested&, V& var, const PathView& ids, const X& value) const
    {
        typedef typename boost::mpl::deref<Nested>::type Head;

        if (*ids.current == Head::id)
            return Assign(Head::GetVariable(var), PathView(ids.path, ids.current + 1), value);
        else
            return AssignToNested(typename boost::mpl::next<Nested>::type(), var, ids, value);
    }

    template <typename V, typename X>
    bool AssignToNested(const boost::mpl::l_iter<boost::mpl::l_end>&, V& /*var*/, const PathView& /*ids*/, const X& /*value*/) const
    {
        return false;
    }

    
    // Separate AssignToField overloads for bonded<T>, basic types and containers allows us 
    // to use simpler predicates in boost::mpl::copy_if. This doesn't matter for runtime code
    // but compiles significantly faster.
    template <typename Reader, typename V, typename X>
    bool AssignToField(V& var, uint16_t id, const bonded<X, Reader>& value) const
    {
        return AssignToField(typename boost::mpl::begin<typename nested_fields<V>::type>::type(), var, id, value);
    }

    
    template <typename Reader, typename V, typename X>
    bool AssignToField(V& var, uint16_t id, const value<X, Reader>& value) const
    {
        return AssignToField(typename boost::mpl::begin<typename matching_fields<V, X>::type>::type(), var, id, value);
    }


    template <typename Reader, typename V>
    bool AssignToField(V& var, uint16_t id, const value<void, Reader>& value) const
    {
        return AssignToField(typename boost::mpl::begin<typename container_fields<V>::type>::type(), var, id, value);
    }

    
    template <typename Fields, typename V, typename X>
    bool AssignToField(const Fields&, V& var, uint16_t id, const X& value) const
    {
        typedef typename boost::mpl::deref<Fields>::type Head;

        if (id == Head::id)
        {
            AssignToVar(Head::GetVariable(var), value);
            return false;
        }
        else
        {
            return AssignToField(typename boost::mpl::next<Fields>::type(), var, id, value);
        }
    }

    
    template <typename V, typename X>
    bool AssignToField(const boost::mpl::l_iter<boost::mpl::l_end>&, V& /*var*/, uint16_t /*id*/, const X& /*value*/) const
    {
        return false;
    }


    template <typename V, typename X>
    void AssignToVar(V& var, const X& value) const
    {
        value.Deserialize(var);
    }

    
    template <typename V, typename X>
    void AssignToVar(maybe<V>& var, const X& value) const
    {
        value.Deserialize(var.set_value());
    }
};

} // namespace detail


template <typename T>
class MapTo
    : public detail::MapTo
{
public:
    BOOST_STATIC_ASSERT(has_schema<T>::value);

    MapTo(T& var, const Mappings& mappings)
        : _var(var),
          _mappings(mappings)
    {}

    
    template <typename X>
    bool Base(const X& value) const
    {
        Mappings::const_iterator it = _mappings.find(mapping_base);

        if (it != _mappings.end())
            return Apply(MapTo(_var, it->second.fields), value);
        else
            return false;
    }

    template <typename Reader, typename X>
    bool Field(uint16_t id, const Metadata& /*metadata*/, const bonded<X, Reader>& value) const
    {
        BOOST_ASSERT(id != mapping_base);

        Mappings::const_iterator it = _mappings.find(id);

        if (it != _mappings.end())
        {
            if (!it->second.fields.empty())
                return Apply(MapTo(_var, it->second.fields), value);
            
            if (!it->second.path.empty())
                return Assign(_var, it->second.path, value);
        }
        
        return false;
    }

    template <typename X>
    bool Field(uint16_t id, const Metadata& /*metadata*/, const X& value) const
    {
        BOOST_ASSERT(id != mapping_base);

        Mappings::const_iterator it = _mappings.find(id);

        if (it != _mappings.end() && !it->second.path.empty())
            return Assign(_var, it->second.path, value);
        else
            return false;
    }

private:
    T&               _var;
    const Mappings&  _mappings;
};

} // namespace bond
