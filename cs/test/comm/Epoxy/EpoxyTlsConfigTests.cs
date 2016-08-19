// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using System;
    using Bond.Comm.Epoxy;
    using NUnit.Framework;

    [TestFixture]
    public class EpoxyTlsConfigTests
    {
        [Test]
        public void ServerConfig_NullCertificate_Throws()
        {
            Assert.That(() => new EpoxyServerTlsConfig(null), Throws.InstanceOf<ArgumentNullException>());
        }
    }
}
