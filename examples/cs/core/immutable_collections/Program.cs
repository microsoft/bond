namespace Examples
{
    using Bond;
    using Bond.IO.Unsafe;
    using Bond.Protocols;
    using System;
    using System.Collections.Generic;
    using System.Collections.Immutable;
    using System.Linq;

    static class Program
    {
        static void Main(string[] args)
        {
            var random = new Random();
            var ints = new int[100];
            for (int i = 0; i < ints.Length; i++)
            {
                ints[i] = random.Next();
            }

            var strings = new string[100];
            for (int i = 0; i < strings.Length; i++)
            {
                strings[i] = $"string{i}";
            }

            var stringToStringMap = strings.Select((str) => new KeyValuePair<string, string>(str, str));
            var intToStringMap = strings.Select((str, idx) => new KeyValuePair<int, string>(idx, str));

            var src = new ImmutableCollectionsHolder()
            {
                ImmutableArrayOfStrings = ImmutableArray.CreateRange(strings),
                ImmutableHashSetOfStrings = ImmutableHashSet.CreateRange(strings),
                ImmutableListOfStrings = ImmutableList.CreateRange(strings),
                ImmutableSortedIntToStringDictionary = ImmutableSortedDictionary.CreateRange(intToStringMap),
                ImmutableSortedSetOfInts = ImmutableSortedSet.CreateRange(ints),
                ImmutableStringToStringDictionary = ImmutableDictionary.CreateRange(stringToStringMap),
            };

            var outputBuffer = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(outputBuffer);
            Serialize.To(writer, src);

            var inputBuffer = new InputBuffer(outputBuffer.Data);
            var reader = new CompactBinaryReader<InputBuffer>(inputBuffer);
            var dst = Deserialize<ImmutableCollectionsHolder>.From(reader);
            ThrowIfFalse(Comparer.Equal(src, dst));
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
