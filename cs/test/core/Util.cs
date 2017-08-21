namespace UnitTest
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Reflection;
    using System.Runtime.InteropServices;
    using System.Text;
    using System.Xml;
    using RMarshal = System.Runtime.InteropServices.Marshal;
    using NUnit.Framework;
    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;
    using Marshal = Bond.Marshal;

    public static class Util
    {
        private const int UnsafeBufferSize = 4 * 1024 * 1024;

        public static IEnumerable<MethodInfo> GetDeclaredMethods(this Type type, string name)
        {
            return type.GetMethods().Where(m => m.Name == name);
        }

        public static MethodInfo GetMethod(this Type type, string name, params Type[] paramTypes)
        {
            var methods = type.GetDeclaredMethods(name);

            return (
                from method in methods
                let parameters = method.GetParameters()
                where parameters != null
                where parameters.Select(p => p.ParameterType).Where(t => !t.IsGenericParameter).SequenceEqual(paramTypes)
                select method).FirstOrDefault();
        }

        public static void TranscodeCBCB(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new CompactBinaryReader<InputStream>(input);

            var output = new OutputStream(to, 19);
            var writer = new CompactBinaryWriter<OutputStream>(output);

            Transcode.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeCB2CB2(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new CompactBinaryReader<InputStream>(input, 2);

            var output = new OutputStream(to, 19);
            var writer = new CompactBinaryWriter<OutputStream>(output, 2);

            Transcode.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeCBFB(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new CompactBinaryReader<InputStream>(input);

            var output = new OutputStream(to, 19);
            var writer = new FastBinaryWriter<OutputStream>(output);

            Transcode.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeCB2FB(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new CompactBinaryReader<InputStream>(input, 2);

            var output = new OutputStream(to, 19);
            var writer = new FastBinaryWriter<OutputStream>(output);

            Transcode.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeCBSP<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new CompactBinaryReader<InputStream>(input);

            var output = new OutputStream(to, 19);
            var writer = new SimpleBinaryWriter<OutputStream>(output);

            Transcode<From>.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeCBXml<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new CompactBinaryReader<InputStream>(input);

            var hasBase = Schema<From>.RuntimeSchema.HasBase;
            var writer = new SimpleXmlWriter(to, new SimpleXmlWriter.Settings { UseNamespaces = hasBase });

            Transcode<From>.FromTo(reader, writer);
            writer.Flush();
        }

        public static void TranscodeCBJson<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new CompactBinaryReader<InputStream>(input);

            var writer = new SimpleJsonWriter(to);

            Transcode<From>.FromTo(reader, writer);
            writer.Flush();
        }

        public static void TranscodeFBCB(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new FastBinaryReader<InputStream>(input);

            var output = new OutputStream(to, 19);
            var writer = new CompactBinaryWriter<OutputStream>(output);

            Transcode.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeFBFB(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new FastBinaryReader<InputStream>(input);

            var output = new OutputStream(to, 19);
            var writer = new FastBinaryWriter<OutputStream>(output);

            Transcode.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeFBSP<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new FastBinaryReader<InputStream>(input);

            var output = new OutputStream(to, 19);
            var writer = new SimpleBinaryWriter<OutputStream>(output);

            Transcode<From>.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeFBXml<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new FastBinaryReader<InputStream>(input);

            var hasBase = Schema<From>.RuntimeSchema.HasBase;
            var writer = new SimpleXmlWriter(to, new SimpleXmlWriter.Settings { UseNamespaces = hasBase });

            Transcode<From>.FromTo(reader, writer);
            writer.Flush();
        }

        public static void TranscodeFBJson<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new FastBinaryReader<InputStream>(input);

            var writer = new SimpleJsonWriter(to);

            Transcode<From>.FromTo(reader, writer);
            writer.Flush();
        }

        public static void TranscodeSPSP<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new SimpleBinaryReader<InputStream>(input);

            var output = new OutputStream(to, 19);
            var writer = new SimpleBinaryWriter<OutputStream>(output);

            Transcode<From>.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeSPCB<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new SimpleBinaryReader<InputStream>(input);

            var output = new OutputStream(to, 19);
            var writer = new CompactBinaryWriter<OutputStream>(output);

            Transcode<From>.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeSPCB2<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new SimpleBinaryReader<InputStream>(input);

            var output = new OutputStream(to, 19);
            var writer = new CompactBinaryWriter<OutputStream>(output, 2);

            Transcode<From>.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeSPFB<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new SimpleBinaryReader<InputStream>(input);

            var output = new OutputStream(to, 19);
            var writer = new FastBinaryWriter<OutputStream>(output);

            Transcode<From>.FromTo(reader, writer);
            output.Flush();
        }

        public static void TranscodeSPXml<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new SimpleBinaryReader<InputStream>(input);

            var hasBase = Schema<From>.RuntimeSchema.HasBase;
            var writer = new SimpleXmlWriter(to, new SimpleXmlWriter.Settings { UseNamespaces = hasBase });

            Transcode<From>.FromTo(reader, writer);
            writer.Flush();
        }

        public static void TranscodeSPJson<From>(Stream from, Stream to)
        {
            var input = new InputStream(from, 11);
            var reader = new SimpleBinaryReader<InputStream>(input);

            var writer = new SimpleJsonWriter(to);

            Transcode<From>.FromTo(reader, writer);
            writer.Flush();
        }

        // CompactBinary v1:

        public static void SerializeCB<T>(T obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output);

            Serialize.To(writer, obj);
            output.Flush();
        }

        public static void MarshalCB<T>(T obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output);

            Marshal.To(writer, obj);
            output.Flush();
        }

        public static void SerializerMarshalCB<T>(T obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output);
            var serializer = new Serializer<CompactBinaryWriter<OutputStream>>(typeof(T));
            serializer.Marshal(obj, writer);
            output.Flush();
        }

        public static ArraySegment<byte> MarshalCB<T>(T obj)
        {
            var output = new OutputBuffer(new byte[11]);
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Marshal.To(writer, obj);
            return output.Data;
        }

        public static void SerializeCB<T>(IBonded<T> obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output);

            Serialize.To(writer, obj);
            output.Flush();
        }

        public static void SerializeCB(IBonded obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output);

            Serialize.To(writer, obj);
            output.Flush();
        }

        public static ArraySegment<byte> SerializeUnsafeCB<T>(T obj)
        {
            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, obj);
            return output.Data;
        }

        public static IntPtr SerializePointerCB<T>(T obj, IntPtr ptr, int length)
        {
            var output = new OutputPointer(ptr, length);
            var writer = new CompactBinaryWriter<OutputPointer>(output);

            Serialize.To(writer, obj);
            return output.Data;
        }

        public static ArraySegment<byte> SerializeSafeCB<T>(T obj)
        {
            var output = new Bond.IO.Safe.OutputBuffer(new byte[11]);
            var writer = new CompactBinaryWriter<Bond.IO.Safe.OutputBuffer>(output);

            Serialize.To(writer, obj);
            return output.Data;
        }

        public static ArraySegment<byte> SerializeSafeCBNoInlining<T>(T obj)
        {
            var output = new Bond.IO.Safe.OutputBuffer(new byte[11]);
            var writer = new CompactBinaryWriter<Bond.IO.Safe.OutputBuffer>(output);

            var serializer = new Serializer<CompactBinaryWriter<Bond.IO.Safe.OutputBuffer>>(typeof(T), false);
            serializer.Serialize(obj, writer);
            return output.Data;
        }

        public static T DeserializeCB<T>(Stream stream)
        {
            var input = new InputStream(stream);
            var reader = new CompactBinaryReader<InputStream>(input);

            return Deserialize<T>.From(reader);
        }

        public static T DeserializeSafeCB<T>(ArraySegment<byte> data)
        {
            var input = new Bond.IO.Safe.InputBuffer(data);
            var reader = new CompactBinaryReader<Bond.IO.Safe.InputBuffer>(input);

            return Deserialize<T>.From(reader);
        }

        public static T DeserializeUnsafeCB<T>(ArraySegment<byte> data)
        {
            var input = new InputBuffer(data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            return Deserialize<T>.From(reader);
        }

        public static T DeserializePointerCB<T>(IntPtr data, int length)
        {
            var input = new InputPointer(data, length);
            var reader = new CompactBinaryReader<InputPointer>(input);

            return Deserialize<T>.From(reader);
        }

        // CompactBinary v2:

        public static void SerializeCB2<T>(T obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output, 2);

            Serialize.To(writer, obj);
            output.Flush();
        }

        public static void SerializeCB2Multiple<T>(T obj, Stream stream, uint times)
        {
            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output, 2);

            for (uint i = 0; i < times; i++)
            {
                Serialize.To(writer, obj);

                // Verify that CBv2 writer has an active FirstPassWriter before the next top-level serialization
                Assert.NotNull(writer.GetFirstPassWriter());
            }
            output.Flush();
        }

        public static void MarshalCB2<T>(T obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output, 2);

            Marshal.To(writer, obj);
            output.Flush();
        }

        public static void SerializerMarshalCB2<T>(T obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output, 2);
            var serializer = new Serializer<CompactBinaryWriter<OutputStream>>(typeof(T));
            serializer.Marshal(obj, writer);
            output.Flush();
        }

        public static ArraySegment<byte> MarshalCB2<T>(T obj)
        {
            var output = new OutputBuffer(new byte[11]);
            var writer = new CompactBinaryWriter<OutputBuffer>(output, 2);

            Marshal.To(writer, obj);
            return output.Data;
        }

        public static void SerializeCB2<T>(IBonded<T> obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output, 2);

            Serialize.To(writer, obj);
            output.Flush();
        }

        public static void SerializeCB2(IBonded obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output, 2);

            Serialize.To(writer, obj);
            output.Flush();
        }

        public static ArraySegment<byte> SerializeUnsafeCB2<T>(T obj)
        {
            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output, 2);

            Serialize.To(writer, obj);
            return output.Data;
        }

        public static IntPtr SerializePointerCB2<T>(T obj, IntPtr ptr, int length)
        {
            var output = new OutputPointer(ptr, length);
            var writer = new CompactBinaryWriter<OutputPointer>(output, 2);

            Serialize.To(writer, obj);
            return output.Data;
        }

        public static ArraySegment<byte> SerializeSafeCB2<T>(T obj)
        {
            var output = new Bond.IO.Safe.OutputBuffer(new byte[11]);
            var writer = new CompactBinaryWriter<Bond.IO.Safe.OutputBuffer>(output, 2);

            Serialize.To(writer, obj);
            return output.Data;
        }

        public static ArraySegment<byte> SerializeSafeCB2NoInlining<T>(T obj)
        {
            var output = new Bond.IO.Safe.OutputBuffer(new byte[11]);
            var writer = new CompactBinaryWriter<Bond.IO.Safe.OutputBuffer>(output, 2);

            var serializer = new Serializer<CompactBinaryWriter<Bond.IO.Safe.OutputBuffer>>(typeof(T), false);
            serializer.Serialize(obj, writer);
            return output.Data;
        }

        public static T DeserializeCB2<T>(Stream stream)
        {
            var input = new InputStream(stream);
            var reader = new CompactBinaryReader<InputStream>(input, 2);

            return Deserialize<T>.From(reader);
        }

        public static T DeserializeSafeCB2<T>(ArraySegment<byte> data)
        {
            var input = new Bond.IO.Safe.InputBuffer(data);
            var reader = new CompactBinaryReader<Bond.IO.Safe.InputBuffer>(input, 2);

            return Deserialize<T>.From(reader);
        }

        public static T DeserializeUnsafeCB2<T>(ArraySegment<byte> data)
        {
            var input = new InputBuffer(data);
            var reader = new CompactBinaryReader<InputBuffer>(input, 2);

            return Deserialize<T>.From(reader);
        }

        public static T DeserializePointerCB2<T>(IntPtr data, int length)
        {
            var input = new InputPointer(data, length);
            var reader = new CompactBinaryReader<InputPointer>(input, 2);

            return Deserialize<T>.From(reader);
        }

        // FastBinary:

        public static void SerializeFB<T>(T obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new FastBinaryWriter<OutputStream>(output);

            Serialize.To(writer, obj);
            output.Flush();
        }

        public static void MarshalFB<T>(T obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new FastBinaryWriter<OutputStream>(output);

            Marshal.To(writer, obj);
            output.Flush();
        }

        public static void SerializerMarshalFB<T>(T obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new FastBinaryWriter<OutputStream>(output);
            var serializer = new Serializer<FastBinaryWriter<OutputStream>>(typeof(T));
            serializer.Marshal(obj, writer);
            output.Flush();
        }

        public static ArraySegment<byte> MarshalFB<T>(T obj)
        {
            var output = new OutputBuffer();
            var writer = new FastBinaryWriter<OutputBuffer>(output);

            Marshal.To(writer, obj);
            return output.Data;
        }

        public static void SerializeFB<T>(IBonded<T> obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new FastBinaryWriter<OutputStream>(output);

            Serialize.To(writer, obj);
            output.Flush();
        }

        public static void SerializeFB(IBonded obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new FastBinaryWriter<OutputStream>(output);

            Serialize.To(writer, obj);
            output.Flush();
        }

        public static ArraySegment<byte> SerializeFB<T>(T obj)
        {
            var output = new OutputBuffer();
            var writer = new FastBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, obj);
            return output.Data;
        }

        public static T DeserializeFB<T>(Stream stream)
        {
            var input = new InputStream(stream);
            var reader = new FastBinaryReader<InputStream>(input);

            return Deserialize<T>.From(reader);
        }

        public static T DeserializeSafeFB<T>(ArraySegment<byte> data)
        {
            var input = new Bond.IO.Safe.InputBuffer(data);
            var reader = new FastBinaryReader<Bond.IO.Safe.InputBuffer>(input);

            return Deserialize<T>.From(reader);
        }

        public static T DeserializeUnsafeFB<T>(ArraySegment<byte> data)
        {
            var input = new InputBuffer(data);
            var reader = new FastBinaryReader<InputBuffer>(input);

            return Deserialize<T>.From(reader);
        }

        public static T DeserializePointerFB<T>(IntPtr data, int length)
        {
            var input = new InputPointer(data, length);
            var reader = new FastBinaryReader<InputPointer>(input);

            return Deserialize<T>.From(reader);
        }

        public static void SerializeSP<T>(T obj, Stream stream, ushort version)
        {
            var output = new OutputStream(stream, 11);
            var writer = new SimpleBinaryWriter<OutputStream>(output, version);

            Serialize.To(writer, obj);
            output.Flush();
        }

        public static void SerializeSP<T>(T obj, Stream stream)
        {
            SerializeSP(obj, stream, 1);
        }

        public static void SerializeSP2<T>(T obj, Stream stream)
        {
            SerializeSP(obj, stream, 2);
        }

        public static void SerializeSP<T>(IBonded<T> obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new SimpleBinaryWriter<OutputStream>(output);

            Serialize.To(writer, obj);
            output.Flush();
        }

        public static ArraySegment<byte> SerializeSP<T>(T obj, ushort version)
        {
            var output = new OutputBuffer();
            var writer = new SimpleBinaryWriter<OutputBuffer>(output, version);

            Serialize.To(writer, obj);
            return output.Data;
        }

        public static ArraySegment<byte> SerializeSP<T>(T obj)
        {
            return SerializeSP(obj, 1);
        }

        public static ArraySegment<byte> SerializeSP2<T>(T obj)
        {
            return SerializeSP(obj, 2);
        }

        public static void SerializeJson<T>(T obj, Stream stream)
        {
            var writer = new SimpleJsonWriter(stream);
            Serialize.To(writer, obj);
            writer.Flush();
        }

        public static void MarshalSP<T>(T obj, Stream stream)
        {
            var output = new OutputStream(stream, 11);
            var writer = new SimpleBinaryWriter<OutputStream>(output);

            Marshal.To(writer, obj);
            output.Flush();
        }

        public static To DeserializeSP<From, To>(Stream stream, ushort version)
        {
            var input = new InputStream(stream);
            var reader = new SimpleBinaryReader<InputStream>(input, version);
            var deserializer = new Deserializer<SimpleBinaryReader<InputStream>>(typeof(To), Schema<From>.RuntimeSchema);

            return deserializer.Deserialize<To>(reader);
        }

        public static To DeserializeSP<From, To>(Stream stream)
        {
            return DeserializeSP<From, To>(stream, 1);
        }

        public static To DeserializeSP2<From, To>(Stream stream)
        {
            return DeserializeSP<From, To>(stream, 2);
        }

        public static To DeserializeSafeSP<From, To>(ArraySegment<byte> data, ushort version)
        {
            var input = new Bond.IO.Safe.InputBuffer(data.Array, data.Offset, data.Count);
            var reader = new SimpleBinaryReader<Bond.IO.Safe.InputBuffer>(input, version);
            var deserializer = new Deserializer<SimpleBinaryReader<Bond.IO.Safe.InputBuffer>>(typeof(To), Schema<From>.RuntimeSchema);

            return deserializer.Deserialize<To>(reader);
        }

        public static To DeserializeSafeSP<From, To>(ArraySegment<byte> data)
        {
            return DeserializeSafeSP<From, To>(data, 1);
        }

        public static To DeserializeSafeSP2<From, To>(ArraySegment<byte> data)
        {
            return DeserializeSafeSP<From, To>(data, 2);
        }

        public static To DeserializeUnsafeSP<From, To>(ArraySegment<byte> data, ushort version)
        {
            var input = new InputBuffer(data);
            var reader = new SimpleBinaryReader<InputBuffer>(input, version);
            var deserializer = new Deserializer<SimpleBinaryReader<InputBuffer>>(typeof(To), Schema<From>.RuntimeSchema);

            return deserializer.Deserialize<To>(reader);
        }

        public static To DeserializeUnsafeSP<From, To>(ArraySegment<byte> data)
        {
            return DeserializeUnsafeSP<From, To>(data, 1);
        }

        public static To DeserializeUnsafeSP2<From, To>(ArraySegment<byte> data)
        {
            return DeserializeUnsafeSP<From, To>(data, 2);
        }

        public static To DeserializePointerSP<From, To>(IntPtr data, int length)
        {
            var input = new InputPointer(data, length);
            var reader = new SimpleBinaryReader<InputPointer>(input);
            var deserializer = new Deserializer<SimpleBinaryReader<InputPointer>>(typeof(To), Schema<From>.RuntimeSchema);

            return deserializer.Deserialize<To>(reader);
        }

        public static void SerializeXml<T>(T obj, Stream stream)
        {
            var hasBase = Schema<T>.RuntimeSchema.HasBase;
            var writer = new SimpleXmlWriter(stream, new SimpleXmlWriter.Settings { UseNamespaces = hasBase });
            Serialize.To(writer, obj);
            writer.Flush();
        }

        public static void SerializeXmlWithNamespaces<T>(T obj, Stream stream)
        {
            var writer = new SimpleXmlWriter(stream, new SimpleXmlWriter.Settings { UseNamespaces = true });
            Serialize.To(writer, obj);
            writer.Flush();
        }

        public static void SerializeXml<T>(IBonded<T> obj, Stream stream)
        {
            var writer = new SimpleXmlWriter(stream, new SimpleXmlWriter.Settings { UseNamespaces = true });
            Serialize.To(writer, obj);
            writer.Flush();
        }

        public static T DeserializeXml<T>(Stream stream)
        {
            var reader = new SimpleXmlReader(stream);
            return Deserialize<T>.From(reader);
        }

        public static T DeserializeJson<T>(Stream stream)
        {
            var reader = new SimpleJsonReader(stream);
            return Deserialize<T>.From(reader);
        }

        public static T DeserializeTagged<T>(IClonableTaggedProtocolReader reader)
        {
            return Deserialize<T>.From(reader);
        }

        public static To DeserializeUntagged<From, To>(IClonableUntaggedProtocolReader reader)
        {
            var deserializer = new Deserializer<IClonableUntaggedProtocolReader>(typeof(To), Schema<From>.RuntimeSchema);
            return deserializer.Deserialize<To>(reader);
        }

        public static string SerializeXmlString<T>(T obj)
        {
            var builder = new StringBuilder();
            var writer = new SimpleXmlWriter(XmlWriter.Create(builder, new XmlWriterSettings { OmitXmlDeclaration = true, Indent = true }));
            Serialize.To(writer, obj);
            writer.Flush();
            return builder.ToString();
        }

        public static IBonded<T> MakeBondedCB<T>(T obj)
        {
            var stream = new MemoryStream();
            SerializeCB(obj, stream);
            stream.Position = 0;
            // Create new MemoryStream at non-zero offset in a buffer
            var buffer = new byte[stream.Length + 2];
            stream.Read(buffer, 1, buffer.Length - 2);
            var input = new InputStream(new MemoryStream(buffer, 1, buffer.Length - 2, false, true));
            var reader = new CompactBinaryReader<InputStream>(input);
            return new Bonded<T, CompactBinaryReader<InputStream>>(reader);
        }

        public static IBonded<T> MakeBondedCB2<T>(T obj)
        {
            var stream = new MemoryStream();
            SerializeCB2(obj, stream);
            stream.Position = 0;
            // Create new MemoryStream at non-zero offset in a buffer
            var buffer = new byte[stream.Length + 2];
            stream.Read(buffer, 1, buffer.Length - 2);
            var input = new InputStream(new MemoryStream(buffer, 1, buffer.Length - 2, false, true));
            var reader = new CompactBinaryReader<InputStream>(input, 2);
            return new Bonded<T, CompactBinaryReader<InputStream>>(reader);
        }

        public static IBonded<T> MakeBondedSP<T>(T obj)
        {
            var stream = new MemoryStream();
            SerializeSP(obj, stream);
            stream.Position = 0;
            // Create new MemoryStream at non-zero offset in a buffer
            var buffer = new byte[stream.Length + 2];
            stream.Read(buffer, 1, buffer.Length - 2);
            var input = new InputStream(new MemoryStream(buffer, 1, buffer.Length - 2, false, true));
            var reader = new SimpleBinaryReader<InputStream>(input);
            return new Bonded<T, SimpleBinaryReader<InputStream>>(reader);
        }

        public delegate void RoundtripStream<From, To>(Action<From, Stream> serialize, Func<Stream, To> deserialize);
        public delegate void MarshalStream<From>(Action<From, Stream> serialize);
        public delegate void MarshalMemory<From>(Func<From, ArraySegment<byte>> serialize);
        public delegate void TranscodeStream<From, To>(Action<From, Stream> serialize, Action<Stream, Stream> transcode, Func<Stream, To> deserialize);
        public delegate void RoundtripMemory<From, To>(Func<From, ArraySegment<byte>> serialize, Func<ArraySegment<byte>, To> deserialize);
        public delegate void RoundtripPointer<From, To>(Func<From, IntPtr, int, IntPtr> serialize, Func<IntPtr, int, To> deserialize);
        public delegate void RoundtripMemoryPointer<From, To>(Func<From, ArraySegment<byte>> serialize, Func<IntPtr, int, To> deserialize);

        public static void AllSerializeDeserialize<From, To>(From from, bool noTranscoding = false)
            where From : class
            where To : class
        {
            RoundtripMemory<From, To> memoryRoundtrip = (serialize, deserialize) =>
            {
                var data = serialize(from);
                var to = deserialize(data);
                Assert.IsTrue(from.IsEqual(to));
            };

            RoundtripPointer<From, To> pointerRoundtrip = (serialize, deserialize) =>
            {
                var ptr = RMarshal.AllocHGlobal(UnsafeBufferSize);
                var data = serialize(from, ptr, UnsafeBufferSize);
                var to = deserialize(data, UnsafeBufferSize);
                Assert.IsTrue(from.IsEqual(to));
                RMarshal.FreeHGlobal(data);
            };

            RoundtripMemoryPointer<From, To> memoryPointerRoundtrip = (serialize, deserialize) =>
            {
                var data = serialize(from);
                var pinned = GCHandle.Alloc(data.Array, GCHandleType.Pinned);
                var to = deserialize(RMarshal.UnsafeAddrOfPinnedArrayElement(data.Array, data.Offset), data.Count);
                Assert.IsTrue(from.IsEqual(to));
                pinned.Free();
            };

            RoundtripStream<From, To> streamRoundtrip = (serialize, deserialize) =>
            {
                var stream = new MemoryStream();
                
                serialize(from, stream);
                stream.Position = 0;
                var to = deserialize(stream);

                Assert.IsTrue(from.IsEqual(to));
            };

            MarshalStream<From> streamMarshal = serialize => streamRoundtrip(serialize, stream =>
            {
                stream.Position = 0;
                return Unmarshal<To>.From(new InputStream(stream));
            });

            MarshalStream<From> streamMarshalSchema = serialize => streamRoundtrip(serialize, stream =>
            {
                stream.Position = 0;
                return Unmarshal.From(new InputStream(stream), Schema<From>.RuntimeSchema).Deserialize<To>();
            });

            MarshalStream<From> streamMarshalNoSchema = serialize => streamRoundtrip(serialize, stream =>
            {
                stream.Position = 0;
                return Unmarshal.From(new InputStream(stream)).Deserialize<To>();
            });

            MarshalMemory<From> memoryMarshal = serialize => memoryRoundtrip(serialize, Unmarshal<To>.From);

            TranscodeStream<From, To> streamTranscode = (serialize, transcode, deserialize) => 
                streamRoundtrip((obj, stream) =>
                {
                    using (var tmp = new MemoryStream())
                    {
                        serialize(obj, tmp);
                        tmp.Position = 0;
                        transcode(tmp, stream);
                    }
                }, deserialize);

            if (noTranscoding)
                streamTranscode = (serialize, transcode, deserialize) => { };

            // Compact Binary
            streamRoundtrip(SerializeCB, DeserializeCB<To>);
            memoryRoundtrip(SerializeUnsafeCB, DeserializeSafeCB<To>);
            memoryRoundtrip(SerializeUnsafeCB, DeserializeUnsafeCB<To>);
            memoryPointerRoundtrip(SerializeUnsafeCB, DeserializePointerCB<To>);
            pointerRoundtrip(SerializePointerCB, DeserializePointerCB<To>);
            memoryRoundtrip(SerializeSafeCB, DeserializeSafeCB<To>);
            memoryRoundtrip(SerializeSafeCB, DeserializeUnsafeCB<To>);
            memoryPointerRoundtrip(SerializeSafeCB, DeserializePointerCB<To>);
            memoryRoundtrip(SerializeSafeCBNoInlining, DeserializeSafeCB<To>);

            streamMarshal(MarshalCB);
            streamMarshal(SerializerMarshalCB);
            streamMarshalSchema(MarshalCB);
            streamMarshalNoSchema(MarshalCB);
            memoryMarshal(MarshalCB);

            streamTranscode(SerializeCB, TranscodeCBCB, DeserializeCB<To>);
            streamTranscode(SerializeCB, TranscodeCBFB, DeserializeFB<To>);

            streamRoundtrip(SerializeCB, stream =>
            {
                var input = new InputStream(stream);
                var reader = new CompactBinaryReader<InputStream>(input);
                return DeserializeTagged<To>(reader);
            });

            // Compact Binary v2
            streamRoundtrip(SerializeCB2, DeserializeCB2<To>);
            memoryRoundtrip(SerializeUnsafeCB2, DeserializeSafeCB2<To>);
            memoryRoundtrip(SerializeUnsafeCB2, DeserializeUnsafeCB2<To>);
            memoryPointerRoundtrip(SerializeUnsafeCB2, DeserializePointerCB2<To>);
            pointerRoundtrip(SerializePointerCB2, DeserializePointerCB2<To>);
            memoryRoundtrip(SerializeSafeCB2, DeserializeSafeCB2<To>);
            memoryRoundtrip(SerializeSafeCB2, DeserializeUnsafeCB2<To>);
            memoryPointerRoundtrip(SerializeSafeCB2, DeserializePointerCB2<To>);
            memoryRoundtrip(SerializeSafeCB2NoInlining, DeserializeSafeCB2<To>);

            streamMarshal(MarshalCB2);
            streamMarshal(SerializerMarshalCB2);
            streamMarshalSchema(MarshalCB2);
            streamMarshalNoSchema(MarshalCB2);
            memoryMarshal(MarshalCB2);

            streamTranscode(SerializeCB2, TranscodeCB2CB2, DeserializeCB2<To>);
            streamTranscode(SerializeCB2, TranscodeCB2FB, DeserializeFB<To>);

            streamRoundtrip(SerializeCB2, stream =>
            {
                var input = new InputStream(stream);
                var reader = new CompactBinaryReader<InputStream>(input, 2);
                return DeserializeTagged<To>(reader);
            });

            // Fast Binary
            streamRoundtrip(SerializeFB, DeserializeFB<To>);
            memoryRoundtrip(SerializeFB, DeserializeSafeFB<To>);
            memoryRoundtrip(SerializeFB, DeserializeUnsafeFB<To>);
            memoryPointerRoundtrip(SerializeFB, DeserializePointerFB<To>);

            streamMarshal(MarshalFB);
            streamMarshal(SerializerMarshalFB);
            streamMarshalSchema(MarshalFB);
            streamMarshalNoSchema(MarshalFB);
            memoryMarshal(MarshalFB);

            streamTranscode(SerializeFB, TranscodeFBFB, DeserializeFB<To>);
            streamTranscode(SerializeFB, TranscodeFBCB, DeserializeCB<To>);

            streamRoundtrip(SerializeFB, stream =>
            {
                var input = new InputStream(stream);
                var reader = new FastBinaryReader<InputStream>(input);
                return DeserializeTagged<To>(reader);
            });

#if SUPPORTS_BIGINTEGER
            const bool hasBigInteger = true;
#else
            const bool hasBigInteger = false;
#endif

            // Simple doesn't support omitting fields
            if (typeof(From) != typeof(Nothing) && typeof(From) != typeof(GenericsWithNothing))
            {
                streamRoundtrip(SerializeSP, DeserializeSP<From, To>);
                memoryRoundtrip(SerializeSP, DeserializeSafeSP<From, To>);
                memoryRoundtrip(SerializeSP, DeserializeUnsafeSP<From, To>);
                memoryPointerRoundtrip(SerializeSP, DeserializePointerSP<From, To>);

                streamRoundtrip(SerializeSP2, DeserializeSP2<From, To>);
                memoryRoundtrip(SerializeSP2, DeserializeSafeSP2<From, To>);
                memoryRoundtrip(SerializeSP2, DeserializeUnsafeSP2<From, To>);

                streamTranscode(SerializeCB, TranscodeCBSP<From>, DeserializeSP<From, To>);
                streamTranscode(SerializeFB, TranscodeFBSP<From>, DeserializeSP<From, To>);
                streamTranscode(SerializeSP, TranscodeSPSP<From>, DeserializeSP<From, To>);
                streamTranscode(SerializeSP, TranscodeSPCB<From>, DeserializeCB<To>);
                streamTranscode(SerializeSP, TranscodeSPCB2<From>, DeserializeCB2<To>);
                streamTranscode(SerializeSP, TranscodeSPFB<From>, DeserializeFB<To>);

                // Pull parser doesn't support bonded<T>
                if (!AnyField<From>(Reflection.IsBonded))
                {
                    streamTranscode(SerializeSP, TranscodeSPXml<From>, DeserializeXml<To>);

                    // NewtonSoft JSON doesn't fully support uint64 in portable profile, so we skip
                    // MaxUInt64 type where BigInteger isn't avaiable
                    if (hasBigInteger || (typeof(From) != typeof (MaxUInt64)))
                    {
                        streamTranscode(SerializeSP, TranscodeSPJson<From>, DeserializeJson<To>);
                    }
                }

                streamRoundtrip(SerializeSP, stream =>
                {
                    var input = new InputStream(stream);
                    var reader = new SimpleBinaryReader<InputStream>(input);
                    return DeserializeUntagged<From, To>(reader);
                });

                streamMarshalSchema(MarshalSP);
            }

            // Pull parser doesn't support bonded<T>
            if (!AnyField<From>(Reflection.IsBonded))
            {
                streamRoundtrip(SerializeXml, DeserializeXml<To>);
                streamTranscode(SerializeCB, TranscodeCBXml<From>, DeserializeXml<To>);
                streamTranscode(SerializeFB, TranscodeFBXml<From>, DeserializeXml<To>);

                // NewtonSoft JSON doesn't fully support uint64 in portable profile, so we skip
                // MaxUInt64 type where BigInteger isn't avaiable
                if (hasBigInteger || (typeof (From) != typeof (MaxUInt64)))
                {
                    streamRoundtrip(SerializeJson, DeserializeJson<To>);
                    streamTranscode(SerializeCB, TranscodeCBJson<From>, DeserializeJson<To>);
                    streamTranscode(SerializeFB, TranscodeFBJson<From>, DeserializeJson<To>);
                }
            }
        }

        delegate bool TypePredicate(Type field);

        static bool AnyField<T>(TypePredicate predicate)
        {
            var types = new HashSet<Type>();
            return AnyField(types, typeof(T), predicate);
        }

        static bool AnyField(HashSet<Type> types, Type type, TypePredicate predicate)
        {
            types.Add(type);
            return type.GetSchemaFields()
                .Select(field => field.MemberType)
                .Where(fieldType => !types.Contains(fieldType))
                .Any(fieldType => predicate(fieldType) || fieldType.IsBondStruct() && AnyField(types, fieldType, predicate));
        }
    }
}
