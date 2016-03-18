// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest
{
    using System;

    using Bond;
    using Bond.Comm;
    using NUnit.Framework;

    [TestFixture]
    class MessageTests
    {
        private static readonly Error AnyError = new Error
        {
            error_code = 1,
        };

        private static readonly SomePayloadType AnyPayload = new SomePayloadType
        {
            int_field = 100,
        };

        [Test]
        public void Construct_WithPayload_IsPayload()
        {
            var msg = new Message<SomePayloadType>(AnyPayload);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayloadType>(deserializedPayload));
        }

        [Test]
        public void FromPayload_WithPayload_IsPayload()
        {
            var msg = Message.FromPayload(AnyPayload);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayloadType>(deserializedPayload));
        }

        [Test]
        public void Construct_WithBonded_IsPayload()
        {
            var bondedPayload = new Bonded<SomePayloadType>(AnyPayload);
            var msg = new Message<SomePayloadType>(bondedPayload);
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayloadType>(deserializedPayload));
        }

        [Test]
        public void Construct_WithDerivedPayload_IsPayload()
        {
            var derivedPayload = new SomeDerivedPayloadType
            {
                int_field = 100,
                bool_field = true,
            };

            var msg = new Message<SomePayloadType>(derivedPayload);
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize<SomeDerivedPayloadType>();
            Assert.IsTrue(derivedPayload.IsEqual<SomeDerivedPayloadType>(deserializedPayload));
        }

        [Test]
        public void FromPayload_WithDerivedPayload_IsPayload()
        {
            var derivedPayload = new SomeDerivedPayloadType
            {
                int_field = 100,
                bool_field = true,
            };

            var msg = Message.FromPayload<SomePayloadType>(derivedPayload);
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize<SomeDerivedPayloadType>();
            Assert.IsTrue(derivedPayload.IsEqual<SomeDerivedPayloadType>(deserializedPayload));
        }

        [Test]
        public void Construct_WithError_IsPayload()
        {
            var msg = new Message<Error>(AnyError);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyError.IsEqual<Error>(deserializedPayload));
        }

        [Test]
        public void FromError_WithError_IsError()
        {
            var msg = Message.FromError<Error>(AnyError);

            Assert.IsTrue(msg.IsError);
            Assert.IsNotNull(msg.Error);
            Assert.IsTrue(AnyError.IsEqual<Error>(msg.Error.Deserialize()));

            Assert.Throws<InvalidOperationException>(() => { var _ = msg.Payload; });
        }

        [Test]
        public void FromError_WithBondedError_IsError()
        {
            var msg = Message.FromError<Error>(new Bonded<Error>(AnyError));

            Assert.IsTrue(msg.IsError);
            Assert.IsNotNull(msg.Error);
            Assert.IsTrue(AnyError.IsEqual<Error>(msg.Error.Deserialize()));

            Assert.Throws<InvalidOperationException>(() => { var _ = msg.Payload; });
        }

        [Schema]
        private class SomePayloadType
        {
            [Bond.Id(0)] public int int_field;
        }

        [Schema]
        private class SomeDerivedPayloadType : SomePayloadType
        {
            [Bond.Id(0)] public bool bool_field;
        }
    }
}
