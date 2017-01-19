
#include "basic_types_apply.h"
#include "basic_types_reflection.h"

namespace tests
{
    
    //
    // Overloads of Apply function with common transforms for BasicTypes.
    // These overloads will be selected using argument dependent lookup
    // before ::bond::Apply function templates.
    //
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value)
    {
        return ::bond::Apply<>(transform, value);
    }

    bool Apply(const ::bond::InitSchemaDef& transform,
               const ::tests::BasicTypes& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }

    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded<void, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value)
    {
        return ::bond::Apply<>(transform, value);
    }

    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value)
    {
        return ::bond::Apply<>(transform, value);
    }

    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }

    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded<void, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value)
    {
        return ::bond::Apply<>(transform, value);
    }

    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value)
    {
        return ::bond::Apply<>(transform, value);
    }

    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }

    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded<void, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value)
    {
        return ::bond::Apply<>(transform, value);
    }

    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value)
    {
        return ::bond::Apply<>(transform, value);
    }

    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value)
    {
        return ::bond::Apply<>(transform, value);
    }
    
} // namespace tests

namespace {
    // this is a dummy definition to make sure that this compilation unit is never empty
    extern bool empty = false;
}
