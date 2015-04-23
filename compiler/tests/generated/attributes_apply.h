
#pragma once

#include "attributes_types.h"
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>


namespace tests
{
    
    //
    // Overloads of Apply function with common transforms for Foo.
    // These overloads will be selected using argument dependent lookup
    // before bond::Apply function templates.
    //
    bool Apply(const bond::To<Foo>& transform,
               const bond::bonded<Foo>& value);

    bool Apply(const bond::InitSchemaDef& transform,
               const Foo& value);
    
    bool Apply(const bond::To<Foo>& transform,
               const bond::bonded<Foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value);

    bool Apply(const bond::To<Foo>& transform,
               const bond::bonded<void, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const Foo& value);

    bool Apply(const bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo>& value);
    
    bool Apply(const bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Serializer<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const Foo& value);

    bool Apply(const bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo>& value);
    
    bool Apply(const bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Marshaler<bond::CompactBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::To<Foo>& transform,
               const bond::bonded<Foo, bond::FastBinaryReader<bond::InputBuffer>&>& value);

    bool Apply(const bond::To<Foo>& transform,
               const bond::bonded<void, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const Foo& value);

    bool Apply(const bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo>& value);
    
    bool Apply(const bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Serializer<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const Foo& value);

    bool Apply(const bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo>& value);
    
    bool Apply(const bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Marshaler<bond::FastBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::To<Foo>& transform,
               const bond::bonded<Foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);

    bool Apply(const bond::To<Foo>& transform,
               const bond::bonded<void, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const Foo& value);

    bool Apply(const bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo>& value);
    
    bool Apply(const bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Serializer<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const Foo& value);

    bool Apply(const bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo>& value);
    
    bool Apply(const bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::CompactBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::FastBinaryReader<bond::InputBuffer>&>& value);
    
    bool Apply(const bond::Marshaler<bond::SimpleBinaryWriter<bond::OutputBuffer> >& transform,
               const bond::bonded<Foo, bond::SimpleBinaryReader<bond::InputBuffer>&>& value);
    
} // namespace tests
