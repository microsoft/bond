namespace ExpressionsTest
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Linq.Expressions;
    using System.Reflection;

    using Bond;
    using Bond.Expressions;
    using Bond.Protocols;
    using Bond.IO.Unsafe;
    using Bond.Internal.Reflection;

    internal static class DebugViewHelper
    {
        static readonly PropertyInfo debugView;

        static DebugViewHelper()
        {
            debugView = typeof(Expression).GetDeclaredProperty("DebugView", typeof(string));
        }

        public static string ToString(Expression expression)
        {
            return (string)debugView.GetValue(expression);
        }

        public static string ToString(IEnumerable<Expression> expressions)
        {
            return string.Concat(expressions.Select(ToString));
        }
    }

    interface IDebugView
    {
        string DebugView { get; }
    }

    internal class Transcoder<R, W> : IDebugView
    {
        readonly string debugView;
        readonly Action<R, W>[] transcode = null;

        public Transcoder(Type type)
            : this(type, ParserFactory<R>.Create(type))
        {}

        public Transcoder(RuntimeSchema schema)
            : this(schema, ParserFactory<R>.Create(schema))
        { }

        public Transcoder()
            : this(new RuntimeSchema())
        {}

        Transcoder(Type type, IParser parser)
        {
            var serializerTransform = SerializerGeneratorFactory<R, W>.Create(
                (r, w, i) => transcode[i](r, w), type);
            var expressions = serializerTransform.Generate(parser);
            debugView = DebugViewHelper.ToString(expressions);
        }

        Transcoder(RuntimeSchema schema, IParser parser)
        {
            var serializerTransform = SerializerGeneratorFactory<R, W>.Create(
                (r, w, i) => transcode[i](r, w), schema);
            var expressions = serializerTransform.Generate(parser);
            debugView = DebugViewHelper.ToString(expressions);
        }

        string IDebugView.DebugView { get { return debugView; } }
    }

    internal class DeserializerDebugView<R> : IDebugView
    {
        readonly string debugView;
        readonly Func<R, object>[] deserialize = null;

        public DeserializerDebugView(Type type)
        {
            var parser = ParserFactory<R>.Create(type);
            var expressions = new DeserializerTransform<R>(
                (r, i) => deserialize[i](r))
                .Generate(parser, type);
            debugView = DebugViewHelper.ToString(expressions);
        }

        string IDebugView.DebugView { get { return debugView; } }
    }

    internal class SerializerDebugView<W> : IDebugView
    {
        readonly string debugView;
        readonly Action<object, W>[] serialize = null;

        public SerializerDebugView(Type type)
        {
            var parser = new ObjectParser(type);
            var serializerTransform = SerializerGeneratorFactory<object, W>.Create(
                (o, w, i) => serialize[i](o, w), type);
            var expressions = serializerTransform.Generate(parser);
            debugView = DebugViewHelper.ToString(expressions);
        }

        string IDebugView.DebugView { get { return debugView; } }
    }

    static class Program
    {
        static void Write(string name, IDebugView codegen)
        {
            using (var file = File.Create(name))
            {
                using (var sw = new StreamWriter(file))
                {
                    sw.Write(codegen.DebugView);
                }
            }
        }

        static void Main(string[] args)
        {
            Write("TranscodeCBCB.expressions",
                new Transcoder<CompactBinaryReader<InputStream>, CompactBinaryWriter<OutputStream>>());

            Write("TranscodeSPCB.expressions",
                new Transcoder<SimpleBinaryReader<InputStream>, CompactBinaryWriter<OutputStream>>(typeof(Example)));

            Write("TranscodeCBSP.expressions",
                new Transcoder<CompactBinaryReader<InputStream>, SimpleBinaryWriter<OutputStream>>(typeof(Example)));

            Write("DeserializeCB.expressions",
                new DeserializerDebugView<CompactBinaryReader<InputStream>>(typeof(Example)));

            Write("DeserializeSP.expressions",
                new DeserializerDebugView<SimpleBinaryReader<InputStream>>(typeof(Example)));

            Write("DeserializeXml.expressions",
                new DeserializerDebugView<SimpleXmlReader>(typeof(Example)));

            Write("DeserializeJson.expressions",
                new DeserializerDebugView<SimpleJsonReader>(typeof(Example)));

            Write("SerializeSP.expressions",
                new SerializerDebugView<SimpleBinaryWriter<OutputStream>>(typeof(Example)));

            Write("SerializeCB.expressions",
                new SerializerDebugView<CompactBinaryWriter<OutputStream>>(typeof(Example)));

            Write("SerializeXml.expressions",
                new SerializerDebugView<SimpleXmlWriter>(typeof(Example)));
        }
    }
}
