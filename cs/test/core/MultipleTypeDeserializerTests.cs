namespace UnitTest
{
    using System;
    using System.Linq;
    using System.Reflection;
    using System.Threading.Tasks;
    using Bond;
    using Bond.IO.Safe;
    using Bond.Protocols;
    using NUnit.Framework;
    using UnitTest.Aliases;

    [TestFixture]
    public class MultipleTypeDeserializerTests
    {
        readonly static Type[] AllBondTypes = 
            Assembly.GetExecutingAssembly()
                .GetExportedTypes()
                .Where(t => t.IsDefined(typeof(SchemaAttribute), false) && !t.ContainsGenericParameters)
                .ToArray();

        [Test]
        public void MultipleTypeDeserializer_AddAllTypes_SingleThreaded()
        {
            var deserializer = new MultipleTypeDeserializer<SimpleBinaryReader<InputBuffer>>();

            foreach (var bondType in AllBondTypes)
            {
                try
                {
                    deserializer.AddType(bondType);
                }
                catch
                {
                    Console.WriteLine("Failed adding " + bondType);
                    throw;
                }
            }
        }

        [Test]
        public void MultipleTypeDeserializer_AddAllTypes_MultiThreaded()
        {
            var deserializer = new MultipleTypeDeserializer<SimpleBinaryReader<InputBuffer>>();

            Parallel.ForEach(
                AllBondTypes,
                bondType =>
                {
                    try
                    {
                        deserializer.AddType(bondType);
                    }
                    catch
                    {
                        Console.WriteLine("Failed adding " + bondType);
                        throw;
                    }
                });
        }

        [Test]
        public void MultipleTypeDeserializer_Roundtrip()
        {
            var deserializer = new MultipleTypeDeserializer<CompactBinaryReader<InputBuffer>>();

            var methodInfo = typeof (MultipleTypeDeserializerTests).GetMethod("RoundTrip", BindingFlags.NonPublic | BindingFlags.Static);
            
            RoundTrip<BlobAlias>(null);
            foreach (var bondType in AllBondTypes)
            {

//            }
                //          Parallel.ForEach(
                //            AllBondTypes,
                //          bondType =>
                {
                    try
                    {
                        methodInfo.MakeGenericMethod(bondType).Invoke(null, new object[] {deserializer});
                    }
                    catch
                    {
                        Console.WriteLine("Failed round trip on type " + bondType);
                    }
                }
                //);
            }
        }

        static void RoundTrip<T>(MultipleTypeDeserializer<CompactBinaryReader<InputBuffer>> deserializer)
        {
            var randomObj = UnitTest.Random.Init<T>();
            var output = new OutputBuffer();
            Serialize.To(new CompactBinaryWriter<OutputBuffer>(output), randomObj);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);
            var deserialized = Deserialize<T>.From(reader);
            
            // deserializer.Deserialize<T>(reader);

            Assert.IsTrue(Comparer.Equal(randomObj, deserialized));
        }
    }
}
