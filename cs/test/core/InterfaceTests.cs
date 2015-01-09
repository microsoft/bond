namespace UnitTest
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    [TestClass]
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

        class Foo : IFoo
        {
            public IList<string> List { get; set; }
        }

        class Bar : IBar<IFoo>
        {
            public IList<IFoo> Foos { get; set; }
            public string Name { get; set; }
            public ArraySegment<byte> NothingBlob { get; set; }
            public ArraySegment<byte> Blob { get; set; }
        }

        class Factory : IFactory
        {
            public object CreateObject(Type type, Type schemaType)
            {
                if (schemaType == typeof (IFoo))
                    return new Foo();

                if (schemaType == typeof(IBar<IFoo>))
                    return new Bar();

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

        [TestMethod]
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

        [TestMethod]
        public void CloningInterfaces()
        {
            var factory = new Factory();
            var cloner = new Cloner<IBar<IFoo>>(typeof(IBar<IFoo>), factory);
            var from = Random.Init<IBar<IFoo>>(factory);

            var to = cloner.Clone<IBar<IFoo>>(from);
            Assert.IsTrue(Comparer.Equal(from, to));
        }
    }
}
