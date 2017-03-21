namespace UnitTest
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.IO;
    using System.Linq;
    using System.Linq.Expressions;
    using NUnit.Framework;
    using Bond;
    using Bond.IO;
    using Bond.Protocols;
    using Bond.IO.Unsafe;
    using Bond.Internal.Reflection;

    [TestFixture]
    public class InterfaceTests
    {
        [Schema]
        interface IFoo
        {
            [Bond.Id(0), Bond.Type(typeof(Bond.Tag.nullable<List<string>>))]
            IList<string> List { get; set; }
        }

        [Schema]
        interface IBar<T>
        {
            [Bond.Id(0), Bond.Type(typeof(Bond.Tag.nullable<IList<Bond.Tag.classT>>))]
            IList<T> Foos { get; set; }

            [Bond.Id(1), Default(null)]
            string Name { get; set; }

            [Bond.Id(2), Default(null)]
            ArraySegment<byte> NothingBlob { get; set; }

            [Bond.Id(3)]
            ArraySegment<byte> Blob { get; set; }
        }

        [Schema]
        interface IWithLazy<T>
        {
            [Bond.Id(0), Bond.Type(typeof(Bond.Tag.bonded<Bond.Tag.classT>))]
            T LazyT { get; set; }

            [Bond.Id(1), Bond.Type(typeof(Bond.Tag.bonded<IFoo>))]
            IFoo LazyFoo { get; set; }
        }

        class Foo : IFoo
        {
            public IList<string> List { get; set; }
        }

        class Lazy<T, R> : IBonded<T>
            where R : ICloneable<R>
        {
            T instance;
            readonly IBonded<T> bonded;
            static readonly Deserializer<R> deserializer = MakeDeserializer<T, R>();
            
            protected Lazy(T instance)
            {
                this.instance = instance;
                this.bonded = new Bonded<T>(instance);
            }

            protected Lazy(IBonded bonded)
            {
                Debug.Assert(!(bonded is Bonded<T>));

                var lazy = bonded as Lazy<T, R>;
                if (lazy != null)
                    instance = lazy.instance;
                
                this.bonded = bonded.Convert<T>();
            }

            public T Deserialize()
            {
                return deserializer.Deserialize(bonded);
            }

            public void Serialize<W>(W writer)
            {
                bonded.Serialize(writer);
            }

            public U Deserialize<U>()
            {
                throw new NotImplementedException();
            }

            public IBonded<U> Convert<U>()
            {
                return bonded.Convert<U>();
            }

            protected T Instance
            {
                get
                {
                    if (instance == null)
                        instance = deserializer.Deserialize(bonded);

                    return instance;
                }
            }
        }

        class LazyFoo<R> : Lazy<IFoo, R>, IFoo
            where R : ICloneable<R>
        {
            public LazyFoo(IFoo instance)
                : base(instance)
            {}

            public LazyFoo(IBonded bonded)
                : base(bonded)
            {}

            public IList<string> List
            {
                get { return Instance.List; }
                set { Instance.List = value; }
            }
        }

        class Bar : IBar<IFoo>
        {
            public IList<IFoo> Foos { get; set; }
            public string Name { get; set; }
            public ArraySegment<byte> NothingBlob { get; set; }
            public ArraySegment<byte> Blob { get; set; }
        }

        class WithLazyFoo : IWithLazy<IFoo>
        {
            public IFoo LazyT { get; set; }

            public IFoo LazyFoo { get; set; }
        }

        class Factory : IFactory
        {
            public object CreateObject(Type type, Type schemaType)
            {
                if (schemaType == typeof(IFoo))
                    return new Foo();

                if (schemaType == typeof(IBar<IFoo>))
                    return new Bar();

                if (schemaType == typeof(IWithLazy<IFoo>))
                    return new WithLazyFoo();

                throw new InvalidOperationException();
            }

            public object CreateContainer(Type type, Type schemaType, int count)
            {
                if (schemaType == typeof(IList<IFoo>))
                    return new List<IFoo>(count);

                if (schemaType == typeof(List<string>))
                    return new List<string>(count);

                throw new InvalidOperationException();
            }
        }

        static Expression Construct<T>(params Expression[] arguments)
        {
            var ctor = typeof(T).GetConstructor(arguments.Select(a => a.Type).ToArray());
            return ctor != null ? Expression.New(ctor, arguments) : null;
        }
        
        static Expression Factory2<R>(Type type, Type schemaType, params Expression[] arguments) 
            where R : ICloneable<R>
        {
            if (schemaType == typeof(Bond.Tag.bonded<IFoo>))
                return Construct<LazyFoo<R>>(arguments);

            if (schemaType == typeof(IFoo))
                return Expression.New(typeof(Foo));

            if (schemaType == typeof(IBar<IFoo>))
                return Expression.New(typeof(Bar));

            if (schemaType == typeof(IWithLazy<IFoo>))
                return Expression.New(typeof(WithLazyFoo));

            if (schemaType == typeof(IList<IFoo>))
                return Construct<List<IFoo>>(arguments);

            return null;
        }

        static Deserializer<R> MakeDeserializer<T, R>()
            where R : ICloneable<R>
        {
            return new Deserializer<R>(
                typeof(T),
                Factory2<R>);
        }

        static Cloner<T> MakeCloner<T, R>()
            where R : ICloneable<R>
        {
            return new Cloner<T>(
                typeof(T),
                Factory2<R>);
        }

        static T MakeRandom<T, R>()
            where R : ICloneable<R>
        {
            return Random.Init<T>(
                Factory2<R>);
        }

        [Test]
        public void DeserializeInterfaces()
        {
            var factory = new Factory();
            var deserializer = new Deserializer<CompactBinaryReader<InputStream>>(typeof(IBar<IFoo>), factory);
            var from = Random.Init<IBar<IFoo>>(factory);

            var stream = new MemoryStream();
            
            Util.SerializeCB(from, stream);
            stream.Position = 0;
            var input = new InputStream(stream);
            var reader = new CompactBinaryReader<InputStream>(input);

            var to = deserializer.Deserialize<IBar<IFoo>>(reader);
            Assert.IsTrue(Comparer.Equal(from, to));
        }

        [Test]
        public void DeserializeInterfacesCB2()
        {
            var factory = new Factory();
            var deserializer = new Deserializer<CompactBinaryReader<InputStream>>(typeof(IBar<IFoo>), factory);
            var from = Random.Init<IBar<IFoo>>(factory);

            var stream = new MemoryStream();

            Util.SerializeCB2(from, stream);
            stream.Position = 0;
            var input = new InputStream(stream);
            var reader = new CompactBinaryReader<InputStream>(input, 2);

            var to = deserializer.Deserialize<IBar<IFoo>>(reader);
            Assert.IsTrue(Comparer.Equal(from, to));
        }

        [Test]
        public void DeserializeWithLazy()
        {
            var deserializer = MakeDeserializer<IWithLazy<IFoo>, CompactBinaryReader<InputStream>>();
            IWithLazy<IFoo> from = new WithLazyFoo
            {
                LazyT = new LazyFoo<CompactBinaryReader<InputStream>>(MakeRandom<IFoo, CompactBinaryReader<InputStream>>()),
                LazyFoo = new LazyFoo<CompactBinaryReader<InputStream>>(MakeRandom<IFoo, CompactBinaryReader<InputStream>>())
            };

            if (from.LazyFoo.List != null)
                from.LazyFoo.List.Add("end");

            var stream = new MemoryStream();

            Util.SerializeCB(from, stream);
            stream.Position = 0;
            var input = new InputStream(stream);
            var reader = new CompactBinaryReader<InputStream>(input);

            var to = deserializer.Deserialize<IWithLazy<IFoo>>(reader);
            Assert.IsTrue(Comparer.Equal(from, to));
        }

        [Test]
        public void DeserializeWithLazyCB2()
        {
            var deserializer = MakeDeserializer<IWithLazy<IFoo>, CompactBinaryReader<InputStream>>();
            IWithLazy<IFoo> from = new WithLazyFoo
            {
                LazyT = new LazyFoo<CompactBinaryReader<InputStream>>(MakeRandom<IFoo, CompactBinaryReader<InputStream>>()),
                LazyFoo = new LazyFoo<CompactBinaryReader<InputStream>>(MakeRandom<IFoo, CompactBinaryReader<InputStream>>())
            };

            if (from.LazyFoo.List != null)
                from.LazyFoo.List.Add("end");

            var stream = new MemoryStream();

            Util.SerializeCB2(from, stream);
            stream.Position = 0;
            var input = new InputStream(stream);
            var reader = new CompactBinaryReader<InputStream>(input, 2);

            var to = deserializer.Deserialize<IWithLazy<IFoo>>(reader);
            Assert.IsTrue(Comparer.Equal(from, to));
        }

        [Test]
        public void CloningInterfaces()
        {
            var factory = new Factory();
            var cloner = new Cloner<IBar<IFoo>>(typeof(IBar<IFoo>), factory);
            var from = Random.Init<IBar<IFoo>>(factory);

            var to = cloner.Clone<IBar<IFoo>>(from);
            Assert.IsTrue(Comparer.Equal(from, to));
        }

        [Test]
        public void CloningWithLazy()
        {
            var cloner = MakeCloner<IWithLazy<IFoo>, CompactBinaryReader<InputStream>>();
            IWithLazy<IFoo> from = new WithLazyFoo
            {
                LazyT = new LazyFoo<CompactBinaryReader<InputStream>>(MakeRandom<IFoo, CompactBinaryReader<InputStream>>()),
                LazyFoo = new LazyFoo<CompactBinaryReader<InputStream>>(MakeRandom<IFoo, CompactBinaryReader<InputStream>>())
            };

            var to = cloner.Clone<IWithLazy<IFoo>>(from);
            Assert.IsTrue(Comparer.Equal(from, to));
        }
    }
}
