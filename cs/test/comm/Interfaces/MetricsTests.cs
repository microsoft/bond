// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Interfaces
{
    using Bond.Comm;
    using NUnit.Framework;

    [TestFixture]
    class MetricsTests
    {
        public static readonly Metrics BlackHole = new Metrics(null);
    }
}
