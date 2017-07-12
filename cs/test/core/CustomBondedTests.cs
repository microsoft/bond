// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest
{
    using System;
    using System.Linq.Expressions;
    using System.Reflection;
    using System.Threading;
    using Bond;
    using Bond.Expressions;
    using Bond.IO;
    using Bond.IO.Unsafe;
    using Bond.Protocols;
    using Bond.Internal.Reflection;
    using NUnit.Framework;

    [TestFixture]
    public class CustomBondedTests
    {
        #region Schemas

        [Schema]
        public class X
        {
            public X()
                : this("CustomBondedTests.X", "X")
            {
            }

            protected X(string fullName, string name)
            {
                bonded_Y = CustomBonded<Y>.Empty;
            }

            [Bond.Id(0), Bond.Type(typeof (Bond.Tag.bonded<Y>))]
            public CustomBonded<Y> bonded_Y { get; set; }
        }

        [Schema]
        public class Y
        {
            public Y()
                : this("CustomBondedTests.Y", "Y")
            {
            }

            protected Y(string fullName, string name)
            {
                FullName = fullName;
            }

            [Bond.Id(0), Bond.Type(typeof (string))]
            public string FullName { get; set; }
        }

        [Schema]
        public class YDerived : Y
        {
            public YDerived()
                : this("CustomBondedTests.YDerived", "YDerived")
            {
            }

            protected YDerived(string fullName, string name)
                : base(fullName, name)
            {
                Z = CustomBonded<Z>.From(Bond.GenericFactory.Create<Z>());
            }

            [Bond.Id(1), Bond.Type(typeof (Bond.Tag.bonded<Z>))]
            public CustomBonded<Z> Z { get; set; }
        }

        [Schema]
        public class Z
        {
            public Z()
                : this("CustomBondedTests.Z", "Z")
            {
            }

            protected Z(string fullName, string name)
            {
                FullName = fullName;
            }

            [Bond.Id(0)]
            public string FullName { get; set; }

            [Bond.Id(1)]
            public int Value { get; set; }
        }

        #endregion

        public abstract class CustomBonded<T>
        {
            public abstract T Value { get; }

            public abstract CustomBonded<U> Convert<U>();

            public static CustomBonded<T> Empty
            {
                get { return CustomBondedPoly<T, T>.Empty; }
            }

            public static CustomBonded<T> From<TActual>(TActual instance)
            {
                return new CustomBondedPoly<T,TActual>(instance);
            }
        }

        internal class CustomBondedPoly<T, TActual> : CustomBonded<T>, IBonded<T>
        {
            public new static readonly CustomBonded<T> Empty = new CustomBondedPoly<T, TActual>(GenericFactory.Create<TActual>());

            private readonly TActual instance;

            public CustomBondedPoly(TActual instance)
            {
                this.instance = instance;
            }

            public override T Value
            {
                get { return Deserialize(); }
            }

            public T Deserialize()
            {
                return CustomTransformFactory.Instance.Cloner<TActual, T>().Clone<T>(instance);
            }

            public void Serialize<W>(W writer)
            {
                CustomTransformFactory.Instance.Serializer<W, TActual>().Serialize(instance, writer);
            }

            public U Deserialize<U>()
            {
                return CustomTransformFactory.Instance.Cloner<TActual, U>().Clone<U>(instance);
            }

            IBonded<U> IBonded.Convert<U>()
            {
                return this as IBonded<U>;
            }

            public override CustomBonded<U> Convert<U>()
            {
                return new CustomBondedPoly<U, TActual>(instance);
            }
        }

        internal class CustomBonded<T, R> : CustomBonded<T>, IBonded<T>
            where R : ICloneable<R>
        {
            private readonly R reader;
            private readonly RuntimeSchema schema;
            private readonly Lazy<T> value;

            public CustomBonded(R reader)
            {
                this.reader = reader.Clone();
                this.schema = RuntimeSchema.Empty;

                this.value = new Lazy<T>(this.Deserialize<T>, LazyThreadSafetyMode.ExecutionAndPublication);
            }

            public CustomBonded(R reader, RuntimeSchema schema)
            {
                this.reader = reader.Clone();
                this.schema = schema;

                this.value = new Lazy<T>(this.Deserialize<T>, LazyThreadSafetyMode.ExecutionAndPublication);
            }

            public override T Value
            {
                get { return value.Value; }
            }

            public T Deserialize()
            {
                return Deserialize<T>();
            }

            public void Serialize<W>(W writer)
            {
                CustomTransformFactory.Instance.Transcoder<R, W>(schema).Transcode(reader.Clone(), writer);
            }

            public U Deserialize<U>()
            {
                return CustomTransformFactory.Instance.Deserializer<R, U>(schema).Deserialize<U>(reader.Clone());
            }

            IBonded<U> IBonded.Convert<U>()
            {
                return (IBonded<U>) Convert<U>();
            }

            public override CustomBonded<U> Convert<U>()
            {
                return new CustomBonded<U, R>(reader, schema);
            }
        }

        internal class CustomBondedVoid<R> : IBonded
            where R : ICloneable<R>
        {
            private readonly R reader;
            private readonly RuntimeSchema schema;

            public CustomBondedVoid(R reader, RuntimeSchema schema)
            {
                this.reader = reader.Clone();
                this.schema = schema;
            }

            public void Serialize<W>(W writer)
            {
                CustomTransformFactory.Instance.Transcoder<R, W>(schema).Transcode(reader.Clone(), writer);
            }

            public U Deserialize<U>()
            {
                return CustomTransformFactory.Instance.Deserializer<R, U>(schema).Deserialize<U>(reader.Clone());
            }

            IBonded<U> IBonded.Convert<U>()
            {
                return new CustomBonded<U, R>(reader, schema);
            }
        }

        /// <summary>
        ///     Custom ITransformFactory for making Deserializer work.
        /// </summary>
        private class CustomTransformFactory
        {
            public static readonly CustomTransformFactory Instance = new CustomTransformFactory();

            private CustomTransformFactory() { }

            public Cloner<TSource> Cloner<TSource, T>()
            {
                return new Cloner<TSource>(typeof(T), new ObjectParser(typeof(TSource), ObjectBondedFactory), Factory);
            }

            public Serializer<W> Serializer<W, T>()
            {
                return new Serializer<W>(typeof(T), new ObjectParser(typeof(T), ObjectBondedFactory), false);
            }

            public Deserializer<R> Deserializer<R, T>(RuntimeSchema schema)
            {
                var parser = schema.HasValue
                                 ? ParserFactory<R>.Create(schema, PayloadBondedFactory)
                                 : ParserFactory<R>.Create(typeof(T), PayloadBondedFactory);

                return new Deserializer<R>(typeof(T), parser, Factory, false);
            }

            private static Expression ObjectBondedFactory(Type objectType, Expression value)
            {
                var method = objectType.GetMethod(typeof(CustomBonded<>), "From", value.Type);

                return Expression.Call(method, value);
            }

            private static Expression PayloadBondedFactory(Expression reader, Expression schema)
            {
                var ctor = typeof(CustomBondedVoid<>).MakeGenericType(reader.Type).GetConstructor(reader.Type, schema.Type);
                return Expression.New(ctor, reader, schema);
            }

            public Transcoder<R, W> Transcoder<R, W>(RuntimeSchema schema)
            {
                return new Transcoder<R, W>(schema, ParserFactory<R>.Create(schema, PayloadBondedFactory));
            }

            private static Expression Factory(Type type, Type schemaType, params Expression[] arguments)
            {
                if (type.IsGenericType())
                {
                    var typeDefinition = type.GetGenericTypeDefinition();
                    if (typeDefinition == typeof(CustomBonded<>))
                    {
                        var arg = arguments[0]; // CustomBondedVoid<R>

                        Type[] genericArgs = type.GetTypeInfo().GenericTypeArguments;
                        Assert.IsNotEmpty(genericArgs);

                        var bondedConvert = typeof(IBonded).GetMethod("Convert").MakeGenericMethod(genericArgs);

                        return Expression.ConvertChecked(Expression.Call(arg, bondedConvert), type);
                    }
                }

                return null;
            }
        }

        [Test]
        public void SupportSettingCustomFactory_RoundTrip()
        {
            var y = new YDerived();
            y.Z = CustomBonded<Z>.From(new Z {Value = 42});
            var x = new X();
            x.bonded_Y = CustomBonded<Y>.From(y);

            var buffer = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(buffer);
            CustomTransformFactory.Instance.Serializer<CompactBinaryWriter<OutputBuffer>, X>().Serialize(x, writer);

            var inputStream = new InputBuffer(buffer.Data);
            var reader = new CompactBinaryReader<InputBuffer>(inputStream);
            var x1 = CustomTransformFactory.Instance.Deserializer<CompactBinaryReader<InputBuffer>, X>(RuntimeSchema.Empty).Deserialize<X>(reader);

            Assert.That(x1, Is.Not.Null);
            Assert.That(x1.bonded_Y, Is.InstanceOf<CustomBonded<Y, CompactBinaryReader<InputBuffer>>>());
            Assert.That(x1.bonded_Y.Value.FullName, Is.EqualTo("CustomBondedTests.YDerived"));
            Assert.That(x1.bonded_Y.Convert<YDerived>().Value.Z.Value.Value, Is.EqualTo(42));

        }

        [Test]
        public void SupportSettingCustomFactory_Clone()
        {
            var y = new YDerived();
            y.Z = CustomBonded<Z>.From(new Z { Value = 42 });
            var x = new X();
            x.bonded_Y = CustomBonded<Y>.From(y);

            var x1 = CustomTransformFactory.Instance.Cloner<X, X>().Clone<X>(x);

            Assert.That(x1, Is.Not.Null);
            Assert.That(x1.bonded_Y.Value.FullName, Is.EqualTo("CustomBondedTests.YDerived"));
            Assert.That(x1.bonded_Y.Convert<YDerived>().Value.Z.Value.Value, Is.EqualTo(42));
        }
    }
}
