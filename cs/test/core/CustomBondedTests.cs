// ----------------------------------------------------------------------
//  <copyright file="CustomBondedTests.cs" company="Microsoft">
//        Copyright 2016 (c) Microsoft Corporation. All Rights Reserved.
//        Information Contained Herein is Proprietary and Confidential.
//  </copyright>
// ----------------------------------------------------------------------
namespace UnitTest
{
    using System;
    using System.IO;
    using System.Linq.Expressions;
    using System.Reflection;
    using System.Text;
    using System.Threading;
    using Bond;
    using Bond.IO;
    using Bond.IO.Unsafe;
    using Bond.Protocols;
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
                    return CustomTransformFactory.Default.Cloner<TActual, T>().Clone<T>(instance);
                }

                public void Serialize<W>(W writer)
                {
                    CustomTransformFactory.Default.Serializer<W, TActual>().Serialize(instance, writer);
                }

                public U Deserialize<U>()
                {
                    return CustomTransformFactory.Default.Cloner<TActual, U>().Clone<U>(instance);
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
        }

        internal class CustomBonded<T, R> : CustomBonded<T>, IBonded<T> 
            where R : ICloneable<R>
        {
            private readonly R reader;
            private readonly RuntimeSchema schema;

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

            private readonly Lazy<T> value;

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
                CustomTransformFactory.Default.Transcoder<R, W>(schema).Transcode(reader.Clone(), writer);
            }

            public U Deserialize<U>()
            {
                return CustomTransformFactory.Default.Deserializer<R, U>(schema).Deserialize<U>(reader.Clone());
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

            public CustomBondedVoid(R reader)
                : this(reader, RuntimeSchema.Empty)
            {
            }

            public CustomBondedVoid(R reader, RuntimeSchema schema)
            {
                this.reader = reader.Clone();
                this.schema = schema;
            }

            public void Serialize<W>(W writer)
            {
                CustomTransformFactory.Default.Transcoder<R, W>(schema).Transcode(reader.Clone(), writer);
            }

            public U Deserialize<U>()
            {
                return CustomTransformFactory.Default.Deserializer<R, U>(schema).Deserialize<U>(reader.Clone());
            }

            IBonded<U> IBonded.Convert<U>()
            {
                return new CustomBonded<U, R>(reader, schema);
            }
        }

        /// <summary>
        ///     Custom conversion for making Serializer work
        /// </summary>
        public class BondTypeAliasConverter
        {
            public static IBonded<T> Convert<T>(CustomBonded<T> value, IBonded<T> unused)
            {
                return value as IBonded<T>;
            }

            public static CustomBonded<T> Convert<T>(IBonded<T> value, CustomBonded<T> unused)
            {
                throw new NotSupportedException();
            }
        }

        /// <summary>
        ///     Custom ITransformFactory for making Deserializer work.
        /// </summary>
        private class CustomTransformFactory
        {
            public  static readonly CustomTransformFactory Default = new CustomTransformFactory();

            private CustomTransformFactory()
            {
            }

            public Cloner<TSource> Cloner<TSource, T>()
            {
                return new Cloner<TSource>(typeof (T), Factory);
            }

            public Serializer<W> Serializer<W, T>()
            {
                return new Serializer<W>(typeof (T), false , Factory);
            }

            public Deserializer<R> Deserializer<R, T>(RuntimeSchema schema)
            {
                return new Deserializer<R>(typeof (T), schema, Factory);
            }

            public Transcoder<R,W> Transcoder<R, W>(RuntimeSchema schema)
            {
                if (schema.HasValue)
                    return new Transcoder<R, W>(schema);

                return new Transcoder<R,W>();
            }

            private static Expression Factory(Type type, Type schemaType, params Expression[] arguments)
            {
                if (type.IsGenericType )
                {
                    var typeDefinition = type.GetGenericTypeDefinition();
                    if (typeDefinition == typeof(CustomBonded<>))
                    {
                        var arg = arguments[0]; // CustomBondedVoid<R>

                        var bondedConvert = typeof(IBonded).GetMethod("Convert").MakeGenericMethod(type.GetGenericArguments());

                        return Expression.ConvertChecked(Expression.Call(arg, bondedConvert), type);
                    }
                }

                if (type == typeof (IBonded))
                    return CreateCustomBondedVoidExpression(type, arguments);

                return null;
            }

            private static Expression CreateCustomBondedVoidExpression(Type type, Expression[] arguments)
            {
                if (arguments.Length == 1)
                {
                    var reader = arguments[0]; // should be of type R

                    var ctor = typeof(CustomBondedVoid<>).MakeGenericType(reader.Type).GetConstructor(new[] { reader.Type });
                    return Expression.New(ctor, reader);
                }
                if (arguments.Length == 2)
                {
                    var reader = arguments[0];
                    var schema = arguments[1];

                    var ctor = typeof(CustomBondedVoid<>).MakeGenericType(reader.Type).GetConstructor(new[] {reader.Type, typeof(RuntimeSchema)});

                    return Expression.New(ctor, reader, schema);
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

            var stream = new MemoryStream();
            var outputStream = new OutputStream(stream);
            var writer = new CompactBinaryWriter<OutputStream>(outputStream);
            CustomTransformFactory.Default.Serializer<CompactBinaryWriter<OutputStream>, X>().Serialize(x, writer);
            outputStream.Flush();

            stream.Position = 0;
            var inputStream = new InputStream(stream);
            var reader = new CompactBinaryReader<InputStream>(inputStream);
            var x1 = CustomTransformFactory.Default.Deserializer<CompactBinaryReader<InputStream>, X>(RuntimeSchema.Empty).Deserialize<X>(reader);

            Assert.That(x1, Is.Not.Null);
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

            var x1 = CustomTransformFactory.Default.Cloner<X, X>().Clone<X>(x);

            Assert.That(x1, Is.Not.Null);
            Assert.That(x1.bonded_Y.Value.FullName, Is.EqualTo("CustomBondedTests.YDerived"));
            Assert.That(x1.bonded_Y.Convert<YDerived>().Value.Z.Value.Value, Is.EqualTo(42));
        }
    }
}
