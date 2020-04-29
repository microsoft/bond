
#pragma once

#include "basic_types_types.h"
#include <bond/core/reflection.h>

namespace tests
{
    //
    // BasicTypes
    //
    struct BasicTypes::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s__bool_metadata;
        private: static const ::bond::Metadata s__str_metadata;
        private: static const ::bond::Metadata s__wstr_metadata;
        private: static const ::bond::Metadata s__uint64_metadata;
        private: static const ::bond::Metadata s__uint16_metadata;
        private: static const ::bond::Metadata s__uint32_metadata;
        private: static const ::bond::Metadata s__uint8_metadata;
        private: static const ::bond::Metadata s__int8_metadata;
        private: static const ::bond::Metadata s__int16_metadata;
        private: static const ::bond::Metadata s__int32_metadata;
        private: static const ::bond::Metadata s__int64_metadata;
        private: static const ::bond::Metadata s__double_metadata;
        private: static const ::bond::Metadata s__float_metadata;
        private: static const ::bond::Metadata s__blob_metadata;

        public: struct var
        {
            // _bool
            typedef struct _bool_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                bool,
                &BasicTypes::_bool,
                &s__bool_metadata
            > {} _bool;
        
            // _str
            typedef struct _str_type : ::bond::reflection::FieldTemplate<
                2,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                std::basic_string<char, std::char_traits<char>, typename std::allocator_traits<arena>::template rebind_alloc<char> >,
                &BasicTypes::_str,
                &s__str_metadata
            > {} _str;
        
            // _wstr
            typedef struct _wstr_type : ::bond::reflection::FieldTemplate<
                3,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                std::basic_string<wchar_t, std::char_traits<wchar_t>, typename std::allocator_traits<arena>::template rebind_alloc<wchar_t> >,
                &BasicTypes::_wstr,
                &s__wstr_metadata
            > {} _wstr;
        
            // _uint64
            typedef struct _uint64_type : ::bond::reflection::FieldTemplate<
                10,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                uint64_t,
                &BasicTypes::_uint64,
                &s__uint64_metadata
            > {} _uint64;
        
            // _uint16
            typedef struct _uint16_type : ::bond::reflection::FieldTemplate<
                11,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                uint16_t,
                &BasicTypes::_uint16,
                &s__uint16_metadata
            > {} _uint16;
        
            // _uint32
            typedef struct _uint32_type : ::bond::reflection::FieldTemplate<
                12,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                uint32_t,
                &BasicTypes::_uint32,
                &s__uint32_metadata
            > {} _uint32;
        
            // _uint8
            typedef struct _uint8_type : ::bond::reflection::FieldTemplate<
                13,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                uint8_t,
                &BasicTypes::_uint8,
                &s__uint8_metadata
            > {} _uint8;
        
            // _int8
            typedef struct _int8_type : ::bond::reflection::FieldTemplate<
                14,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                int8_t,
                &BasicTypes::_int8,
                &s__int8_metadata
            > {} _int8;
        
            // _int16
            typedef struct _int16_type : ::bond::reflection::FieldTemplate<
                15,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                int16_t,
                &BasicTypes::_int16,
                &s__int16_metadata
            > {} _int16;
        
            // _int32
            typedef struct _int32_type : ::bond::reflection::FieldTemplate<
                16,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                int32_t,
                &BasicTypes::_int32,
                &s__int32_metadata
            > {} _int32;
        
            // _int64
            typedef struct _int64_type : ::bond::reflection::FieldTemplate<
                17,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                int64_t,
                &BasicTypes::_int64,
                &s__int64_metadata
            > {} _int64;
        
            // _double
            typedef struct _double_type : ::bond::reflection::FieldTemplate<
                18,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                double,
                &BasicTypes::_double,
                &s__double_metadata
            > {} _double;
        
            // _float
            typedef struct _float_type : ::bond::reflection::FieldTemplate<
                20,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                float,
                &BasicTypes::_float,
                &s__float_metadata
            > {} _float;
        
            // _blob
            typedef struct _blob_type : ::bond::reflection::FieldTemplate<
                21,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                ::bond::blob,
                &BasicTypes::_blob,
                &s__blob_metadata
            > {} _blob;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::_blob>::type fields1;
        private: typedef boost::mpl::push_front<fields1, var::_float>::type fields2;
        private: typedef boost::mpl::push_front<fields2, var::_double>::type fields3;
        private: typedef boost::mpl::push_front<fields3, var::_int64>::type fields4;
        private: typedef boost::mpl::push_front<fields4, var::_int32>::type fields5;
        private: typedef boost::mpl::push_front<fields5, var::_int16>::type fields6;
        private: typedef boost::mpl::push_front<fields6, var::_int8>::type fields7;
        private: typedef boost::mpl::push_front<fields7, var::_uint8>::type fields8;
        private: typedef boost::mpl::push_front<fields8, var::_uint32>::type fields9;
        private: typedef boost::mpl::push_front<fields9, var::_uint16>::type fields10;
        private: typedef boost::mpl::push_front<fields10, var::_uint64>::type fields11;
        private: typedef boost::mpl::push_front<fields11, var::_wstr>::type fields12;
        private: typedef boost::mpl::push_front<fields12, var::_str>::type fields13;
        private: typedef boost::mpl::push_front<fields13, var::_bool>::type fields14;

        public: typedef fields14::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("BasicTypes", "tests.BasicTypes",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
