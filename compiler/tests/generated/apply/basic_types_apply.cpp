
#include "basic_types_apply.h"
#include "basic_types_reflection.h"

namespace bond
{
    
    //
    // Extern template specializations of Apply function with common
    // transforms for BasicTypes.
    //

    template 
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);

    template 
    bool Apply< ::tests::BasicTypes>(const ::bond::InitSchemaDef& transform);

    template 
    bool Apply(const ::bond::Null& transform,
               const ::bond::bonded< ::tests::BasicTypes, ::bond::SimpleBinaryReader< ::bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);

    template 
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded<void, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    template 
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    template 
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::tests::BasicTypes& value);

    template 
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::tests::BasicTypes& value);

    template 
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);

    template 
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded<void, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    template 
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    template 
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);

    template 
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded<void, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    template 
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    template 
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    template 
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
} // namespace bond
