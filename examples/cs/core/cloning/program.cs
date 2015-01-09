namespace Examples
{
    using System.Diagnostics;
    using Bond;

    static class Program
    {
        static void Main()
        {
            var document = new Document
            {
                Title = "FooBar",
                Authors = { "BillG" },
                Tags = { "Memo", "Lorem" },
                Content =
                {
                    "Lorem ipsum dolor sit amet, consectetur adipisicing elit, ",
                    "sed do eiusmod tempor incididunt ut labore et dolore magna ",
                    "aliqua. Ut enim ad minim veniam, quis nostrud exercitation ",
                    "ullamco laboris nisi ut aliquip ex ea commodo consequat."
                }
            };

            // Create an instance of Outline by cloning appropriate fields of the Document object.
            // The soruce and clone don't have to be the same type, merely compatible accoring
            // to normal Bond schema versioning rules.
            var outline = Clone<Outline>.From(document);

            Debug.Assert(outline.Title == document.Title);
            Debug.Assert(outline.Tags.First.Value == document.Tags[0]);
        }
    }
}
