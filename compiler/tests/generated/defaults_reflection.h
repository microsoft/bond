
#pragma once

#include "defaults_types.h"
#include <bond/core/reflection.h>

namespace tests
{
    //
    // Foo
    //
    struct Foo::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_m_bool_1_metadata;
        private: static const ::bond::Metadata s_m_bool_2_metadata;
        private: static const ::bond::Metadata s_m_bool_3_metadata;
        private: static const ::bond::Metadata s_m_str_1_metadata;
        private: static const ::bond::Metadata s_m_str_2_metadata;
        private: static const ::bond::Metadata s_m_int8_4_metadata;
        private: static const ::bond::Metadata s_m_int8_5_metadata;
        private: static const ::bond::Metadata s_m_int16_4_metadata;
        private: static const ::bond::Metadata s_m_int16_5_metadata;
        private: static const ::bond::Metadata s_m_int32_4_metadata;
        private: static const ::bond::Metadata s_m_int32_max_metadata;
        private: static const ::bond::Metadata s_m_int64_4_metadata;
        private: static const ::bond::Metadata s_m_int64_max_metadata;
        private: static const ::bond::Metadata s_m_uint8_2_metadata;
        private: static const ::bond::Metadata s_m_uint8_3_metadata;
        private: static const ::bond::Metadata s_m_uint16_2_metadata;
        private: static const ::bond::Metadata s_m_uint16_3_metadata;
        private: static const ::bond::Metadata s_m_uint32_3_metadata;
        private: static const ::bond::Metadata s_m_uint32_max_metadata;
        private: static const ::bond::Metadata s_m_uint64_3_metadata;
        private: static const ::bond::Metadata s_m_uint64_max_metadata;
        private: static const ::bond::Metadata s_m_double_3_metadata;
        private: static const ::bond::Metadata s_m_double_4_metadata;
        private: static const ::bond::Metadata s_m_double_5_metadata;
        private: static const ::bond::Metadata s_m_float_3_metadata;
        private: static const ::bond::Metadata s_m_float_4_metadata;
        private: static const ::bond::Metadata s_m_float_7_metadata;
        private: static const ::bond::Metadata s_m_enum1_metadata;
        private: static const ::bond::Metadata s_m_enum2_metadata;
        private: static const ::bond::Metadata s_m_enum3_metadata;
        private: static const ::bond::Metadata s_m_enum_int32min_metadata;
        private: static const ::bond::Metadata s_m_enum_int32max_metadata;
        private: static const ::bond::Metadata s_m_enum_uint32_min_metadata;
        private: static const ::bond::Metadata s_m_enum_uint32_max_metadata;
        private: static const ::bond::Metadata s_m_wstr_1_metadata;
        private: static const ::bond::Metadata s_m_wstr_2_metadata;

        public: struct var
        {
            // m_bool_1
            typedef ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo,
                bool,
                &Foo::m_bool_1,
                &s_m_bool_1_metadata
            > m_bool_1;
        
            // m_bool_2
            typedef ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                Foo,
                bool,
                &Foo::m_bool_2,
                &s_m_bool_2_metadata
            > m_bool_2;
        
            // m_bool_3
            typedef ::bond::reflection::FieldTemplate<
                2,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<bool>,
                &Foo::m_bool_3,
                &s_m_bool_3_metadata
            > m_bool_3;
        
            // m_str_1
            typedef ::bond::reflection::FieldTemplate<
                3,
                ::bond::reflection::optional_field_modifier,
                Foo,
                std::string,
                &Foo::m_str_1,
                &s_m_str_1_metadata
            > m_str_1;
        
            // m_str_2
            typedef ::bond::reflection::FieldTemplate<
                4,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<std::string>,
                &Foo::m_str_2,
                &s_m_str_2_metadata
            > m_str_2;
        
            // m_int8_4
            typedef ::bond::reflection::FieldTemplate<
                5,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int8_t,
                &Foo::m_int8_4,
                &s_m_int8_4_metadata
            > m_int8_4;
        
            // m_int8_5
            typedef ::bond::reflection::FieldTemplate<
                6,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<int8_t>,
                &Foo::m_int8_5,
                &s_m_int8_5_metadata
            > m_int8_5;
        
            // m_int16_4
            typedef ::bond::reflection::FieldTemplate<
                7,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int16_t,
                &Foo::m_int16_4,
                &s_m_int16_4_metadata
            > m_int16_4;
        
            // m_int16_5
            typedef ::bond::reflection::FieldTemplate<
                8,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<int16_t>,
                &Foo::m_int16_5,
                &s_m_int16_5_metadata
            > m_int16_5;
        
            // m_int32_4
            typedef ::bond::reflection::FieldTemplate<
                9,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<int32_t>,
                &Foo::m_int32_4,
                &s_m_int32_4_metadata
            > m_int32_4;
        
            // m_int32_max
            typedef ::bond::reflection::FieldTemplate<
                10,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int32_t,
                &Foo::m_int32_max,
                &s_m_int32_max_metadata
            > m_int32_max;
        
            // m_int64_4
            typedef ::bond::reflection::FieldTemplate<
                11,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<int64_t>,
                &Foo::m_int64_4,
                &s_m_int64_4_metadata
            > m_int64_4;
        
            // m_int64_max
            typedef ::bond::reflection::FieldTemplate<
                12,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int64_t,
                &Foo::m_int64_max,
                &s_m_int64_max_metadata
            > m_int64_max;
        
            // m_uint8_2
            typedef ::bond::reflection::FieldTemplate<
                13,
                ::bond::reflection::optional_field_modifier,
                Foo,
                uint8_t,
                &Foo::m_uint8_2,
                &s_m_uint8_2_metadata
            > m_uint8_2;
        
            // m_uint8_3
            typedef ::bond::reflection::FieldTemplate<
                14,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<uint8_t>,
                &Foo::m_uint8_3,
                &s_m_uint8_3_metadata
            > m_uint8_3;
        
            // m_uint16_2
            typedef ::bond::reflection::FieldTemplate<
                15,
                ::bond::reflection::optional_field_modifier,
                Foo,
                uint16_t,
                &Foo::m_uint16_2,
                &s_m_uint16_2_metadata
            > m_uint16_2;
        
            // m_uint16_3
            typedef ::bond::reflection::FieldTemplate<
                16,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<uint16_t>,
                &Foo::m_uint16_3,
                &s_m_uint16_3_metadata
            > m_uint16_3;
        
            // m_uint32_3
            typedef ::bond::reflection::FieldTemplate<
                17,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<uint32_t>,
                &Foo::m_uint32_3,
                &s_m_uint32_3_metadata
            > m_uint32_3;
        
            // m_uint32_max
            typedef ::bond::reflection::FieldTemplate<
                18,
                ::bond::reflection::optional_field_modifier,
                Foo,
                uint32_t,
                &Foo::m_uint32_max,
                &s_m_uint32_max_metadata
            > m_uint32_max;
        
            // m_uint64_3
            typedef ::bond::reflection::FieldTemplate<
                19,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<uint64_t>,
                &Foo::m_uint64_3,
                &s_m_uint64_3_metadata
            > m_uint64_3;
        
            // m_uint64_max
            typedef ::bond::reflection::FieldTemplate<
                20,
                ::bond::reflection::optional_field_modifier,
                Foo,
                uint64_t,
                &Foo::m_uint64_max,
                &s_m_uint64_max_metadata
            > m_uint64_max;
        
            // m_double_3
            typedef ::bond::reflection::FieldTemplate<
                21,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<double>,
                &Foo::m_double_3,
                &s_m_double_3_metadata
            > m_double_3;
        
            // m_double_4
            typedef ::bond::reflection::FieldTemplate<
                22,
                ::bond::reflection::optional_field_modifier,
                Foo,
                double,
                &Foo::m_double_4,
                &s_m_double_4_metadata
            > m_double_4;
        
            // m_double_5
            typedef ::bond::reflection::FieldTemplate<
                23,
                ::bond::reflection::optional_field_modifier,
                Foo,
                double,
                &Foo::m_double_5,
                &s_m_double_5_metadata
            > m_double_5;
        
            // m_float_3
            typedef ::bond::reflection::FieldTemplate<
                24,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<float>,
                &Foo::m_float_3,
                &s_m_float_3_metadata
            > m_float_3;
        
            // m_float_4
            typedef ::bond::reflection::FieldTemplate<
                25,
                ::bond::reflection::optional_field_modifier,
                Foo,
                float,
                &Foo::m_float_4,
                &s_m_float_4_metadata
            > m_float_4;
        
            // m_float_7
            typedef ::bond::reflection::FieldTemplate<
                26,
                ::bond::reflection::optional_field_modifier,
                Foo,
                float,
                &Foo::m_float_7,
                &s_m_float_7_metadata
            > m_float_7;
        
            // m_enum1
            typedef ::bond::reflection::FieldTemplate<
                27,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum1,
                &s_m_enum1_metadata
            > m_enum1;
        
            // m_enum2
            typedef ::bond::reflection::FieldTemplate<
                28,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum2,
                &s_m_enum2_metadata
            > m_enum2;
        
            // m_enum3
            typedef ::bond::reflection::FieldTemplate<
                29,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe< ::tests::EnumType1>,
                &Foo::m_enum3,
                &s_m_enum3_metadata
            > m_enum3;
        
            // m_enum_int32min
            typedef ::bond::reflection::FieldTemplate<
                30,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum_int32min,
                &s_m_enum_int32min_metadata
            > m_enum_int32min;
        
            // m_enum_int32max
            typedef ::bond::reflection::FieldTemplate<
                31,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum_int32max,
                &s_m_enum_int32max_metadata
            > m_enum_int32max;
        
            // m_enum_uint32_min
            typedef ::bond::reflection::FieldTemplate<
                32,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum_uint32_min,
                &s_m_enum_uint32_min_metadata
            > m_enum_uint32_min;
        
            // m_enum_uint32_max
            typedef ::bond::reflection::FieldTemplate<
                33,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum_uint32_max,
                &s_m_enum_uint32_max_metadata
            > m_enum_uint32_max;
        
            // m_wstr_1
            typedef ::bond::reflection::FieldTemplate<
                34,
                ::bond::reflection::optional_field_modifier,
                Foo,
                std::wstring,
                &Foo::m_wstr_1,
                &s_m_wstr_1_metadata
            > m_wstr_1;
        
            // m_wstr_2
            typedef ::bond::reflection::FieldTemplate<
                35,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<std::wstring>,
                &Foo::m_wstr_2,
                &s_m_wstr_2_metadata
            > m_wstr_2;
        };

        private: typedef ::bond::mpl::nil fields0;
        public: struct fields1 { typedef fields0 tail; typedef var::m_wstr_2 field; };
        public: struct fields2 { typedef fields1 tail; typedef var::m_wstr_1 field; };
        public: struct fields3 { typedef fields2 tail; typedef var::m_enum_uint32_max field; };
        public: struct fields4 { typedef fields3 tail; typedef var::m_enum_uint32_min field; };
        public: struct fields5 { typedef fields4 tail; typedef var::m_enum_int32max field; };
        public: struct fields6 { typedef fields5 tail; typedef var::m_enum_int32min field; };
        public: struct fields7 { typedef fields6 tail; typedef var::m_enum3 field; };
        public: struct fields8 { typedef fields7 tail; typedef var::m_enum2 field; };
        public: struct fields9 { typedef fields8 tail; typedef var::m_enum1 field; };
        public: struct fields10 { typedef fields9 tail; typedef var::m_float_7 field; };
        public: struct fields11 { typedef fields10 tail; typedef var::m_float_4 field; };
        public: struct fields12 { typedef fields11 tail; typedef var::m_float_3 field; };
        public: struct fields13 { typedef fields12 tail; typedef var::m_double_5 field; };
        public: struct fields14 { typedef fields13 tail; typedef var::m_double_4 field; };
        public: struct fields15 { typedef fields14 tail; typedef var::m_double_3 field; };
        public: struct fields16 { typedef fields15 tail; typedef var::m_uint64_max field; };
        public: struct fields17 { typedef fields16 tail; typedef var::m_uint64_3 field; };
        public: struct fields18 { typedef fields17 tail; typedef var::m_uint32_max field; };
        public: struct fields19 { typedef fields18 tail; typedef var::m_uint32_3 field; };
        public: struct fields20 { typedef fields19 tail; typedef var::m_uint16_3 field; };
        public: struct fields21 { typedef fields20 tail; typedef var::m_uint16_2 field; };
        public: struct fields22 { typedef fields21 tail; typedef var::m_uint8_3 field; };
        public: struct fields23 { typedef fields22 tail; typedef var::m_uint8_2 field; };
        public: struct fields24 { typedef fields23 tail; typedef var::m_int64_max field; };
        public: struct fields25 { typedef fields24 tail; typedef var::m_int64_4 field; };
        public: struct fields26 { typedef fields25 tail; typedef var::m_int32_max field; };
        public: struct fields27 { typedef fields26 tail; typedef var::m_int32_4 field; };
        public: struct fields28 { typedef fields27 tail; typedef var::m_int16_5 field; };
        public: struct fields29 { typedef fields28 tail; typedef var::m_int16_4 field; };
        public: struct fields30 { typedef fields29 tail; typedef var::m_int8_5 field; };
        public: struct fields31 { typedef fields30 tail; typedef var::m_int8_4 field; };
        public: struct fields32 { typedef fields31 tail; typedef var::m_str_2 field; };
        public: struct fields33 { typedef fields32 tail; typedef var::m_str_1 field; };
        public: struct fields34 { typedef fields33 tail; typedef var::m_bool_3 field; };
        public: struct fields35 { typedef fields34 tail; typedef var::m_bool_2 field; };
        public: struct fields36 { typedef fields35 tail; typedef var::m_bool_1 field; };

        public: typedef fields36 fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
