namespace Examples
{
    using System;

    static class Program
    {
        static void Main()
        {
            var yellow = Color.Yellow;
            ThrowIfFalse((int)yellow == 2);

            var apple = Fruit.Apple;
            ThrowIfFalse((int)apple == 1);

            ThrowIfFalse((Int32)Limits.Int32Min == Int32.MinValue);
            ThrowIfFalse((Int32)Limits.Int32Max == Int32.MaxValue);
            ThrowIfFalse((UInt32)Limits.UInt32Min == UInt32.MinValue);
            ThrowIfFalse(unchecked((UInt32)Limits.UInt32Max) == UInt32.MaxValue);
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
