// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Grpc
{
    using Bond;

    [Schema]
    public class SomePayload
    {
        public const int DefaultIntValue = 50;

        [Bond.Id(0)]
        public int int_field;

        public SomePayload()
        {
            int_field = DefaultIntValue;
        }

        public static readonly SomePayload Any = new SomePayload
        {
            int_field = 100,
        };
    }

    [Schema]
    public class SomeDerivedPayload : SomePayload
    {
        [Bond.Id(0)]
        public bool bool_field;

        public static readonly new SomeDerivedPayload Any = new SomeDerivedPayload
        {
            int_field = 500,
            bool_field = true,
        };
    }
}
