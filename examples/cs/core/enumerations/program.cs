namespace Examples
{
    using System;
    using System.Diagnostics;

    static class Program
    {
        static void Main()
        {
            var yellow = Color.Yellow;
            Debug.Assert((int)yellow == 2);

            var apple = Fruit.Apple;
            Debug.Assert((int)apple == 1);

            Debug.Assert((Int32)Limits.Int32Min == Int32.MinValue);
            Debug.Assert((Int32)Limits.Int32Max == Int32.MaxValue);
            Debug.Assert((UInt32)Limits.UInt32Min == UInt32.MinValue);
            Debug.Assert(unchecked((UInt32)Limits.UInt32Max) == UInt32.MaxValue);
        }
    }
}
