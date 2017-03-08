
#pragma once

#include "basic_types_types.h"
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>


namespace tests
{
    
    //
    // Overloads of Apply function with common transforms for BasicTypes.
    // These overloads will be selected using argument dependent lookup
    // before ::bond::Apply function templates.
    //
    DllExport
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);

    DllExport
    bool Apply(const ::bond::InitSchemaDef& transform,
               const ::tests::BasicTypes& value);

    DllExport
    bool Apply(const ::bond::Null& transform,
               const ::bond::bonded< ::tests::BasicTypes, ::bond::SimpleBinaryReader< ::bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);

    DllExport
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded<void, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    DllExport
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    DllExport
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::tests::BasicTypes& value);

    DllExport
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::tests::BasicTypes& value);

    DllExport
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::CompactBinaryWriter<bond::OutputCounter> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);

    DllExport
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded<void, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    DllExport
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    DllExport
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);

    DllExport
    bool Apply(const ::bond::To< ::tests::BasicTypes>& transform,
               const ::bond::bonded<void, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    DllExport
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::tests::BasicTypes& value);

    DllExport
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    DllExport
    bool Apply(const ::bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const ::bond::bonded< ::tests::BasicTypes, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
} // namespace tests
