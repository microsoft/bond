
namespace UnitTest
{
    using NUnit.Framework;
    using System.Collections.Generic;

    [TestFixture]
    class ReadOnlyCollectionTests
    {
        [Bond.Schema]
           private class DTO
        {
            [Bond.Id(0), Bond.Type(typeof(Dictionary<string, string>))]
            public IReadOnlyDictionary<string, string> d1 { get; set; }

            [Bond.Id(1), Bond.Type(typeof(List<string>))]
            public IReadOnlyCollection<string> d2 { get; set; }

            [Bond.Id(2), Bond.Type(typeof(List<string>))]
            public IEnumerable<string> d3 { get; set; }

            [Bond.Id(3), Bond.Type(typeof(List<string>))]
            public IReadOnlyList<string> d4 { get; set; }

            [Bond.Id(4), Bond.Type(typeof(string[]))]
            public IReadOnlyCollection<string> d5 { get; set; }

            [Bond.Id(5), Bond.Type(typeof(string[]))]
            public IEnumerable<string> d6 { get; set; }

            [Bond.Id(6), Bond.Type(typeof(string[]))]
            public IReadOnlyList<string> d7 { get; set; }

            public bool Equals(DTO dto)
            {
                return d1.IsEqual<IReadOnlyDictionary<string, string>>(dto.d1) &&
                    d2.IsEqual<IReadOnlyCollection<string>>(dto.d2) &&
                    d3.IsEqual<IEnumerable<string>>(dto.d3) &&
                    d4.IsEqual<IReadOnlyList<string>>(dto.d4) &&
                    d5.IsEqual<IReadOnlyCollection<string>>(dto.d5) &&
                    d6.IsEqual<IEnumerable<string>>(dto.d6) &&
                    d7.IsEqual<IReadOnlyList<string>>(dto.d7);
            }
        }


        [Test]
           public void ReadOnlyCollections()
        {
            Util.AllSerializeDeserialize<DTO, DTO>(Random.Init<DTO>());
        }
    }
}
