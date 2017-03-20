
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
            typedef ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                bool,
                &BasicTypes::_bool,
                &s__bool_metadata
            > _bool;
        
            // _str
            typedef ::bond::reflection::FieldTemplate<
                2,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                std::string,
                &BasicTypes::_str,
                &s__str_metadata
            > _str;
        
            // _wstr
            typedef ::bond::reflection::FieldTemplate<
                3,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                std::wstring,
                &BasicTypes::_wstr,
                &s__wstr_metadata
            > _wstr;
        
            // _uint64
            typedef ::bond::reflection::FieldTemplate<
                10,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                uint64_t,
                &BasicTypes::_uint64,
                &s__uint64_metadata
            > _uint64;
        
            // _uint16
            typedef ::bond::reflection::FieldTemplate<
                11,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                uint16_t,
                &BasicTypes::_uint16,
                &s__uint16_metadata
            > _uint16;
        
            // _uint32
            typedef ::bond::reflection::FieldTemplate<
                12,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                uint32_t,
                &BasicTypes::_uint32,
                &s__uint32_metadata
            > _uint32;
        
            // _uint8
            typedef ::bond::reflection::FieldTemplate<
                13,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                uint8_t,
                &BasicTypes::_uint8,
                &s__uint8_metadata
            > _uint8;
        
            // _int8
            typedef ::bond::reflection::FieldTemplate<
                14,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                int8_t,
                &BasicTypes::_int8,
                &s__int8_metadata
            > _int8;
        
            // _int16
            typedef ::bond::reflection::FieldTemplate<
                15,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                int16_t,
                &BasicTypes::_int16,
                &s__int16_metadata
            > _int16;
        
            // _int32
            typedef ::bond::reflection::FieldTemplate<
                16,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                int32_t,
                &BasicTypes::_int32,
                &s__int32_metadata
            > _int32;
        
            // _int64
            typedef ::bond::reflection::FieldTemplate<
                17,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                int64_t,
                &BasicTypes::_int64,
                &s__int64_metadata
            > _int64;
        
            // _double
            typedef ::bond::reflection::FieldTemplate<
                18,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                double,
                &BasicTypes::_double,
                &s__double_metadata
            > _double;
        
            // _float
            typedef ::bond::reflection::FieldTemplate<
                20,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                float,
                &BasicTypes::_float,
                &s__float_metadata
            > _float;
        
            // _blob
            typedef ::bond::reflection::FieldTemplate<
                21,
                ::bond::reflection::optional_field_modifier,
                BasicTypes,
                ::bond::blob,
                &BasicTypes::_blob,
                &s__blob_metadata
            > _blob;
        };

        private: typedef ::bond::mpl::nil fields0;
        public: struct fields1 { typedef fields0 tail; typedef var::_blob field; };
        public: struct fields2 { typedef fields1 tail; typedef var::_float field; };
        public: struct fields3 { typedef fields2 tail; typedef var::_double field; };
        public: struct fields4 { typedef fields3 tail; typedef var::_int64 field; };
        public: struct fields5 { typedef fields4 tail; typedef var::_int32 field; };
        public: struct fields6 { typedef fields5 tail; typedef var::_int16 field; };
        public: struct fields7 { typedef fields6 tail; typedef var::_int8 field; };
        public: struct fields8 { typedef fields7 tail; typedef var::_uint8 field; };
        public: struct fields9 { typedef fields8 tail; typedef var::_uint32 field; };
        public: struct fields10 { typedef fields9 tail; typedef var::_uint16 field; };
        public: struct fields11 { typedef fields10 tail; typedef var::_uint64 field; };
        public: struct fields12 { typedef fields11 tail; typedef var::_wstr field; };
        public: struct fields13 { typedef fields12 tail; typedef var::_str field; };
        public: struct fields14 { typedef fields13 tail; typedef var::_bool field; };

        public: typedef fields14 fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("BasicTypes", "tests.BasicTypes",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
