namespace Examples
{
    using System;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;
    using examples.generic_tree;

    static class Program
    {
        static void Main()
        {
            // Define root node for a tree of strings
            var root = new Node<string> {data = "root"};
            root.left = new Node<string> {data = "root/left"};
            root.right = new Node<string> {data = "root/right"};
            root.left.left = new Node<string> {data = "root/left/left"};
            root.left.left.right = new Node<string> {data = "root/left/left/right"};

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);
            Marshal.To(writer, root);

            var tree = Unmarshal<Node<string>>.From(output.Data);

           ThrowIfFalse(Comparer.Equal(root, tree));
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
