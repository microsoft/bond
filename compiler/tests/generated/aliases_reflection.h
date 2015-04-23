
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
        typedef bond::no_base base;

        static const bond::Metadata metadata;
        
        private: static const bond::Metadata s_aa_metadata;

        public: struct var
        {
            // aa
            typedef bond::reflection::FieldTemplate<
                0,
                bond::reflection::optional_field_modifier,
                Foo<T>,
                std::vector<std::vector<T> >,
                &Foo<T>::aa,
                &s_aa_metadata
            > aa;
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
        
        static bond::Metadata GetMetadata()
        {
            return bond::reflection::MetadataInit<boost::mpl::list<T> >("Foo", "tests.Foo",
                bond::reflection::Attributes()
            );
        }
    };
    
    template <typename T>
    const bond::Metadata Foo<T>::Schema::metadata
        = Foo<T>::Schema::GetMetadata();
    
    template <typename T>
    const bond::Metadata Foo<T>::Schema::s_aa_metadata
        = bond::reflection::MetadataInit("aa");

    
} // namespace tests
