
#pragma once

#include "generics_types.h"
#include <bond/core/reflection.h>

namespace tests
{
    //
    // Foo
    //
    template <typename T1, typename T2>
    struct Foo<T1, T2>::Schema
    {
        typedef ::bond::no_base base;

        static const ::bond::Metadata metadata;
        
        private: static const ::bond::Metadata s_t2_metadata;
        private: static const ::bond::Metadata s_n_metadata;

        public: struct var
        {
            // t2
            typedef struct t2_type : ::bond::reflection::FieldTemplate<
                0,
                ::bond::reflection::optional_field_modifier,
                Foo<T1, T2>,
                T2,
                &Foo<T1, T2>::t2,
                &s_t2_metadata
            > {} t2;
        
            // n
            typedef struct n_type : ::bond::reflection::FieldTemplate<
                1,
                ::bond::reflection::optional_field_modifier,
                Foo<T1, T2>,
                ::bond::nullable< ::tests::Foo<T1, bool> >,
                &Foo<T1, T2>::n,
                &s_n_metadata
            > {} n;
        };

        private: typedef boost::mpl::list<> fields0;
        private: typedef typename boost::mpl::push_front<fields0, typename var::n>::type fields1;
        private: typedef typename boost::mpl::push_front<fields1, typename var::t2>::type fields2;

        public: typedef typename fields2::type fields;
        
        Schema()
        {
            // Force instantiation of template statics
            (void)metadata;
            (void)s_t2_metadata;
            (void)s_n_metadata;
        }
        
        static ::bond::Metadata GetMetadata()
        {
            return ::bond::reflection::MetadataInit<boost::mpl::list<T1, T2> >("Foo", "tests.Foo",
                ::bond::reflection::Attributes()
            );
        }
    };
    
    template <typename T1, typename T2>
    const ::bond::Metadata Foo<T1, T2>::Schema::metadata
        = Foo<T1, T2>::Schema::GetMetadata();
    
    template <typename T1, typename T2>
    const ::bond::Metadata Foo<T1, T2>::Schema::s_t2_metadata
        = ::bond::reflection::MetadataInit("t2");
    
    template <typename T1, typename T2>
    const ::bond::Metadata Foo<T1, T2>::Schema::s_n_metadata
        = ::bond::reflection::MetadataInit("n");

    
} // namespace tests
