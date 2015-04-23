
#include "alias_with_allocator_apply.h"
#include "alias_with_allocator_reflection.h"

namespace test
{
    
    //
    // Overloads of Apply function with common transforms for foo.
    // These overloads will be selected using argument dependent lookup
    // before bond::Apply function templates.
    //
    bool Apply(const bond::To<foo>& transform,
               const bond::bonded<foo>& value)
    {
        return bond::Apply<>(transform, value);
    }

    bool Apply(const bond::InitSchemaDef& transform,
               const foo& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::To<foo>& transform,
               const bond::bonded<foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }

    bool Apply(const bond::To<foo>& transform,
               const bond::bonded<void, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const foo& value)
    {
        return bond::Apply<>(transform, value);
    }

    bool Apply(const bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const foo& value)
    {
        return bond::Apply<>(transform, value);
    }

    bool Apply(const bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::To<foo>& transform,
               const bond::bonded<foo, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }

    bool Apply(const bond::To<foo>& transform,
               const bond::bonded<void, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const foo& value)
    {
        return bond::Apply<>(transform, value);
    }

    bool Apply(const bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const foo& value)
    {
        return bond::Apply<>(transform, value);
    }

    bool Apply(const bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::To<foo>& transform,
               const bond::bonded<foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }

    bool Apply(const bond::To<foo>& transform,
               const bond::bonded<void, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const foo& value)
    {
        return bond::Apply<>(transform, value);
    }

    bool Apply(const bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const foo& value)
    {
        return bond::Apply<>(transform, value);
    }

    bool Apply(const bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
    bool Apply(const bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return bond::Apply<>(transform, value);
    }
    
} // namespace test
