namespace Examples
{
    using System;
    using System.Collections;
    using System.Collections.Generic;

    using Bond;
    using Bond.IO.Unsafe;
    using Bond.Protocols;

    static class Program
    {
        static void Main()
        {
            var src = new Example
            {
                Name = "FooBar",
                Constants = {  3.14, 6.28 }
            };

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, src);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var dst = Deserialize<Example>.From(reader);
            ThrowIfFalse(Bond.Comparer.Equal(src, dst));
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }

    // An extremely simple example of a custom container implementation.
    public class SomeCustomList<T> : ICollection<T>, ICollection
    {
        readonly List<T> backingList = new List<T>();

        public IEnumerator<T> GetEnumerator()
        {
            return backingList.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable)backingList).GetEnumerator();
        }

        public void Add(T item)
        {
            backingList.Add(item);
        }

        public void Clear()
        {
            backingList.Clear();
        }

        public bool Contains(T item)
        {
            return backingList.Contains(item);
        }

        public void CopyTo(T[] array, int arrayIndex)
        {
            backingList.CopyTo(array, arrayIndex);
        }

        public bool Remove(T item)
        {
            return backingList.Remove(item);
        }

        public void CopyTo(Array array, int index)
        {
            ((ICollection)backingList).CopyTo(array, index);
        }

        public int Count
        {
            get { return backingList.Count; }
        }

        public object SyncRoot
        {
            get { return ((ICollection)backingList).SyncRoot; }
        }

        public bool IsSynchronized
        {
            get { return ((ICollection)backingList).IsSynchronized; }
        }

        bool ICollection<T>.IsReadOnly
        {
            get { return ((ICollection<T>) backingList).IsReadOnly; }
        }
    }
}
