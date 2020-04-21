
#pragma once

#include "aliases_types.h"
#include <bond/core/reflection.h>

namespace tests
{
    //
    // Foo
    //
    template <typename T>
    struct Foo<T>::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_aa_metadata;

        public: struct var
        {
            // aa
            typedef struct aa_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo<T>,
                std::vector<std::vector<T, std::scoped_allocator_adaptor<typename std::allocator_traits<arena>::template rebind_alloc<T> > >, std::scoped_allocator_adaptor<typename std::allocator_traits<arena>::template rebind_alloc<std::vector<T, std::scoped_allocator_adaptor<typename std::allocator_traits<arena>::template rebind_alloc<T> > > > > >,
                &Foo<T>::aa,
                &s_aa_metadata
            > {} aa;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef typename boost::mpl::push_front<fields0, typename var::aa>::type fields1;

        public: typedef typename fields1::type fields;
        
        Schema()
        {
            // Force instantiation of template statics
            (void)metadata;
            (void)s_aa_metadata;
        }
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit<boost::mpl::list<T> >("Foo", "tests.Foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    
    template <typename T>
    const ::bond::Metadata Foo<T>::Schema::metadata
        = Foo<T>::Schema::GetMetadata();
    
    template <typename T>
    const ::bond::Metadata Foo<T>::Schema::s_aa_metadata
        = ::bond::reflection::MetadataInit("aa");

    //
    // WrappingAnEnum
    //
    struct WrappingAnEnum::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_aWrappedEnum_metadata;

        public: struct var
        {
            // aWrappedEnum
            typedef struct aWrappedEnum_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                WrappingAnEnum,
                ::tests::EnumToWrap,
                &WrappingAnEnum::aWrappedEnum,
                &s_aWrappedEnum_metadata
            > {} aWrappedEnum;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef boost::mpl::push_front<fields0, var::aWrappedEnum>::type fields1;

        public: typedef fields1::type fields;
        
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit("WrappingAnEnum", "tests.WrappingAnEnum",
                ::bond::reflection::Attributes()
            );
        }
    };
    

    
} // namespace tests
