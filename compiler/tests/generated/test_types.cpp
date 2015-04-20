
#include "test_reflection.h"
#include <bond/core/exception.h>

namespace test
{
    
    const bond::Metadata foo::Schema::metadata
        = foo::Schema::GetMetadata();
    
    const bond::Metadata foo::Schema::s_a_metadata
        = bond::reflection::MetadataInit("re", "a");
    
    const bond::Metadata foo::Schema::s_b_metadata
        = bond::reflection::MetadataInit(static_cast<int16_t>(10), "b");
    
    const bond::Metadata foo::Schema::s_c_metadata
        = bond::reflection::MetadataInit("c");
    
    const bond::Metadata foo::Schema::s_m_metadata
        = bond::reflection::MetadataInit("m");
    
    const bond::Metadata foo::Schema::s_n_metadata
        = bond::reflection::MetadataInit("n");
    
    const bond::Metadata foo::Schema::s_l_metadata
        = bond::reflection::MetadataInit("l");

    
} // namespace test
