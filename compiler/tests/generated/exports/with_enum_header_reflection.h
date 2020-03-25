
#pragma once

#include "with_enum_header_types.h"
#include <bond/core/reflection.h>

namespace tests
{
    //
    // Foo
    //
    struct Foo::Schema
    {
        typedef ::bond::no_base base;

        DllExport
        static const ::bond::Metadata metadata;
        
        private: DllExport
        static const ::bond::Metadata s_m_bool_1_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_bool_2_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_bool_3_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_str_1_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_str_2_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_int8_4_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_int8_5_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_int16_4_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_int16_5_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_int32_4_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_int32_max_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_int64_4_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_int64_max_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_uint8_2_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_uint8_3_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_uint16_2_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_uint16_3_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_uint32_3_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_uint32_max_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_uint64_3_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_uint64_max_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_double_3_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_double_4_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_double_5_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_float_3_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_float_4_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_float_7_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_enum1_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_enum2_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_enum3_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_enum_int32min_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_enum_int32max_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_enum_uint32_min_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_enum_uint32_max_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_wstr_1_metadata;
        private: DllExport
        static const ::bond::Metadata s_m_wstr_2_metadata;

        public: struct var
        {
            // m_bool_1
            struct m_bool_1 : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo,
                bool,
                &Foo::m_bool_1,
                &s_m_bool_1_metadata
            > {};

            struct m_bool_1 m_bool_1;
        
            // m_bool_2
            struct m_bool_2 : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                Foo,
                bool,
                &Foo::m_bool_2,
                &s_m_bool_2_metadata
            > {};

            struct m_bool_2 m_bool_2;
        
            // m_bool_3
            struct m_bool_3 : ::bond::reflection::FieldTemplate<
                2,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<bool>,
                &Foo::m_bool_3,
                &s_m_bool_3_metadata
            > {};

            struct m_bool_3 m_bool_3;
        
            // m_str_1
            struct m_str_1 : ::bond::reflection::FieldTemplate<
                3,
                ::bond::reflection::optional_field_modifier,
                Foo,
                std::string,
                &Foo::m_str_1,
                &s_m_str_1_metadata
            > {};

            struct m_str_1 m_str_1;
        
            // m_str_2
            struct m_str_2 : ::bond::reflection::FieldTemplate<
                4,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<std::string>,
                &Foo::m_str_2,
                &s_m_str_2_metadata
            > {};

            struct m_str_2 m_str_2;
        
            // m_int8_4
            struct m_int8_4 : ::bond::reflection::FieldTemplate<
                5,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int8_t,
                &Foo::m_int8_4,
                &s_m_int8_4_metadata
            > {};

            struct m_int8_4 m_int8_4;
        
            // m_int8_5
            struct m_int8_5 : ::bond::reflection::FieldTemplate<
                6,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<int8_t>,
                &Foo::m_int8_5,
                &s_m_int8_5_metadata
            > {};

            struct m_int8_5 m_int8_5;
        
            // m_int16_4
            struct m_int16_4 : ::bond::reflection::FieldTemplate<
                7,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int16_t,
                &Foo::m_int16_4,
                &s_m_int16_4_metadata
            > {};

            struct m_int16_4 m_int16_4;
        
            // m_int16_5
            struct m_int16_5 : ::bond::reflection::FieldTemplate<
                8,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<int16_t>,
                &Foo::m_int16_5,
                &s_m_int16_5_metadata
            > {};

            struct m_int16_5 m_int16_5;
        
            // m_int32_4
            struct m_int32_4 : ::bond::reflection::FieldTemplate<
                9,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<int32_t>,
                &Foo::m_int32_4,
                &s_m_int32_4_metadata
            > {};

            struct m_int32_4 m_int32_4;
        
            // m_int32_max
            struct m_int32_max : ::bond::reflection::FieldTemplate<
                10,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int32_t,
                &Foo::m_int32_max,
                &s_m_int32_max_metadata
            > {};

            struct m_int32_max m_int32_max;
        
            // m_int64_4
            struct m_int64_4 : ::bond::reflection::FieldTemplate<
                11,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<int64_t>,
                &Foo::m_int64_4,
                &s_m_int64_4_metadata
            > {};

            struct m_int64_4 m_int64_4;
        
            // m_int64_max
            struct m_int64_max : ::bond::reflection::FieldTemplate<
                12,
                ::bond::reflection::optional_field_modifier,
                Foo,
                int64_t,
                &Foo::m_int64_max,
                &s_m_int64_max_metadata
            > {};

            struct m_int64_max m_int64_max;
        
            // m_uint8_2
            struct m_uint8_2 : ::bond::reflection::FieldTemplate<
                13,
                ::bond::reflection::optional_field_modifier,
                Foo,
                uint8_t,
                &Foo::m_uint8_2,
                &s_m_uint8_2_metadata
            > {};

            struct m_uint8_2 m_uint8_2;
        
            // m_uint8_3
            struct m_uint8_3 : ::bond::reflection::FieldTemplate<
                14,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<uint8_t>,
                &Foo::m_uint8_3,
                &s_m_uint8_3_metadata
            > {};

            struct m_uint8_3 m_uint8_3;
        
            // m_uint16_2
            struct m_uint16_2 : ::bond::reflection::FieldTemplate<
                15,
                ::bond::reflection::optional_field_modifier,
                Foo,
                uint16_t,
                &Foo::m_uint16_2,
                &s_m_uint16_2_metadata
            > {};

            struct m_uint16_2 m_uint16_2;
        
            // m_uint16_3
            struct m_uint16_3 : ::bond::reflection::FieldTemplate<
                16,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<uint16_t>,
                &Foo::m_uint16_3,
                &s_m_uint16_3_metadata
            > {};

            struct m_uint16_3 m_uint16_3;
        
            // m_uint32_3
            struct m_uint32_3 : ::bond::reflection::FieldTemplate<
                17,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<uint32_t>,
                &Foo::m_uint32_3,
                &s_m_uint32_3_metadata
            > {};

            struct m_uint32_3 m_uint32_3;
        
            // m_uint32_max
            struct m_uint32_max : ::bond::reflection::FieldTemplate<
                18,
                ::bond::reflection::optional_field_modifier,
                Foo,
                uint32_t,
                &Foo::m_uint32_max,
                &s_m_uint32_max_metadata
            > {};

            struct m_uint32_max m_uint32_max;
        
            // m_uint64_3
            struct m_uint64_3 : ::bond::reflection::FieldTemplate<
                19,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<uint64_t>,
                &Foo::m_uint64_3,
                &s_m_uint64_3_metadata
            > {};

            struct m_uint64_3 m_uint64_3;
        
            // m_uint64_max
            struct m_uint64_max : ::bond::reflection::FieldTemplate<
                20,
                ::bond::reflection::optional_field_modifier,
                Foo,
                uint64_t,
                &Foo::m_uint64_max,
                &s_m_uint64_max_metadata
            > {};

            struct m_uint64_max m_uint64_max;
        
            // m_double_3
            struct m_double_3 : ::bond::reflection::FieldTemplate<
                21,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<double>,
                &Foo::m_double_3,
                &s_m_double_3_metadata
            > {};

            struct m_double_3 m_double_3;
        
            // m_double_4
            struct m_double_4 : ::bond::reflection::FieldTemplate<
                22,
                ::bond::reflection::optional_field_modifier,
                Foo,
                double,
                &Foo::m_double_4,
                &s_m_double_4_metadata
            > {};

            struct m_double_4 m_double_4;
        
            // m_double_5
            struct m_double_5 : ::bond::reflection::FieldTemplate<
                23,
                ::bond::reflection::optional_field_modifier,
                Foo,
                double,
                &Foo::m_double_5,
                &s_m_double_5_metadata
            > {};

            struct m_double_5 m_double_5;
        
            // m_float_3
            struct m_float_3 : ::bond::reflection::FieldTemplate<
                24,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<float>,
                &Foo::m_float_3,
                &s_m_float_3_metadata
            > {};

            struct m_float_3 m_float_3;
        
            // m_float_4
            struct m_float_4 : ::bond::reflection::FieldTemplate<
                25,
                ::bond::reflection::optional_field_modifier,
                Foo,
                float,
                &Foo::m_float_4,
                &s_m_float_4_metadata
            > {};

            struct m_float_4 m_float_4;
        
            // m_float_7
            struct m_float_7 : ::bond::reflection::FieldTemplate<
                26,
                ::bond::reflection::optional_field_modifier,
                Foo,
                float,
                &Foo::m_float_7,
                &s_m_float_7_metadata
            > {};

            struct m_float_7 m_float_7;
        
            // m_enum1
            struct m_enum1 : ::bond::reflection::FieldTemplate<
                27,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum1,
                &s_m_enum1_metadata
            > {};

            struct m_enum1 m_enum1;
        
            // m_enum2
            struct m_enum2 : ::bond::reflection::FieldTemplate<
                28,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum2,
                &s_m_enum2_metadata
            > {};

            struct m_enum2 m_enum2;
        
            // m_enum3
            struct m_enum3 : ::bond::reflection::FieldTemplate<
                29,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe< ::tests::EnumType1>,
                &Foo::m_enum3,
                &s_m_enum3_metadata
            > {};

            struct m_enum3 m_enum3;
        
            // m_enum_int32min
            struct m_enum_int32min : ::bond::reflection::FieldTemplate<
                30,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum_int32min,
                &s_m_enum_int32min_metadata
            > {};

            struct m_enum_int32min m_enum_int32min;
        
            // m_enum_int32max
            struct m_enum_int32max : ::bond::reflection::FieldTemplate<
                31,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum_int32max,
                &s_m_enum_int32max_metadata
            > {};

            struct m_enum_int32max m_enum_int32max;
        
            // m_enum_uint32_min
            struct m_enum_uint32_min : ::bond::reflection::FieldTemplate<
                32,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum_uint32_min,
                &s_m_enum_uint32_min_metadata
            > {};

            struct m_enum_uint32_min m_enum_uint32_min;
        
            // m_enum_uint32_max
            struct m_enum_uint32_max : ::bond::reflection::FieldTemplate<
                33,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::tests::EnumType1,
                &Foo::m_enum_uint32_max,
                &s_m_enum_uint32_max_metadata
            > {};

            struct m_enum_uint32_max m_enum_uint32_max;
        
            // m_wstr_1
            struct m_wstr_1 : ::bond::reflection::FieldTemplate<
                34,
                ::bond::reflection::optional_field_modifier,
                Foo,
                std::wstring,
                &Foo::m_wstr_1,
                &s_m_wstr_1_metadata
            > {};

            struct m_wstr_1 m_wstr_1;
        
            // m_wstr_2
            struct m_wstr_2 : ::bond::reflection::FieldTemplate<
                35,
                ::bond::reflection::optional_field_modifier,
                Foo,
                ::bond::maybe<std::wstring>,
                &Foo::m_wstr_2,
                &s_m_wstr_2_metadata
            > {};

            struct m_wstr_2 m_wstr_2;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, struct var::m_wstr_2>::type fields1;
        private: typedef boost::mpl::push_front<fields1, struct var::m_wstr_1>::type fields2;
        private: typedef boost::mpl::push_front<fields2, struct var::m_enum_uint32_max>::type fields3;
        private: typedef boost::mpl::push_front<fields3, struct var::m_enum_uint32_min>::type fields4;
        private: typedef boost::mpl::push_front<fields4, struct var::m_enum_int32max>::type fields5;
        private: typedef boost::mpl::push_front<fields5, struct var::m_enum_int32min>::type fields6;
        private: typedef boost::mpl::push_front<fields6, struct var::m_enum3>::type fields7;
        private: typedef boost::mpl::push_front<fields7, struct var::m_enum2>::type fields8;
        private: typedef boost::mpl::push_front<fields8, struct var::m_enum1>::type fields9;
        private: typedef boost::mpl::push_front<fields9, struct var::m_float_7>::type fields10;
        private: typedef boost::mpl::push_front<fields10, struct var::m_float_4>::type fields11;
        private: typedef boost::mpl::push_front<fields11, struct var::m_float_3>::type fields12;
        private: typedef boost::mpl::push_front<fields12, struct var::m_double_5>::type fields13;
        private: typedef boost::mpl::push_front<fields13, struct var::m_double_4>::type fields14;
        private: typedef boost::mpl::push_front<fields14, struct var::m_double_3>::type fields15;
        private: typedef boost::mpl::push_front<fields15, struct var::m_uint64_max>::type fields16;
        private: typedef boost::mpl::push_front<fields16, struct var::m_uint64_3>::type fields17;
        private: typedef boost::mpl::push_front<fields17, struct var::m_uint32_max>::type fields18;
        private: typedef boost::mpl::push_front<fields18, struct var::m_uint32_3>::type fields19;
        private: typedef boost::mpl::push_front<fields19, struct var::m_uint16_3>::type fields20;
        private: typedef boost::mpl::push_front<fields20, struct var::m_uint16_2>::type fields21;
        private: typedef boost::mpl::push_front<fields21, struct var::m_uint8_3>::type fields22;
        private: typedef boost::mpl::push_front<fields22, struct var::m_uint8_2>::type fields23;
        private: typedef boost::mpl::push_front<fields23, struct var::m_int64_max>::type fields24;
        private: typedef boost::mpl::push_front<fields24, struct var::m_int64_4>::type fields25;
        private: typedef boost::mpl::push_front<fields25, struct var::m_int32_max>::type fields26;
        private: typedef boost::mpl::push_front<fields26, struct var::m_int32_4>::type fields27;
        private: typedef boost::mpl::push_front<fields27, struct var::m_int16_5>::type fields28;
        private: typedef boost::mpl::push_front<fields28, struct var::m_int16_4>::type fields29;
        private: typedef boost::mpl::push_front<fields29, struct var::m_int8_5>::type fields30;
        private: typedef boost::mpl::push_front<fields30, struct var::m_int8_4>::type fields31;
        private: typedef boost::mpl::push_front<fields31, struct var::m_str_2>::type fields32;
        private: typedef boost::mpl::push_front<fields32, struct var::m_str_1>::type fields33;
        private: typedef boost::mpl::push_front<fields33, struct var::m_bool_3>::type fields34;
        private: typedef boost::mpl::push_front<fields34, struct var::m_bool_2>::type fields35;
        private: typedef boost::mpl::push_front<fields35, struct var::m_bool_1>::type fields36;

        public: typedef fields36::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("Foo", "tests.Foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
