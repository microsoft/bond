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

        private static readonly IBonded AnyTypelessIBonded = (IBonded)new Bonded<SomePayloadType>(AnyPayload);

        private static readonly SomeDerivedPayloadType AnyDerivedPayload = new SomeDerivedPayloadType
        {
            int_field = 500,
            bool_field = true,
        };

        [Test]
        public void Construct_WithPayload_IsPayload()
        {
            var msg = new Message(AnyTypelessIBonded);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.RawPayload.Deserialize<SomePayloadType>();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayloadType>(deserializedPayload));
        }

        [Test]
        public void ConstructT_WithPayload_IsPayload()
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
            var msg = Message.FromPayload(AnyTypelessIBonded);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.RawPayload.Deserialize<SomePayloadType>();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayloadType>(deserializedPayload));
        }

        [Test]
        public void FromPayloadT_WithPayload_IsPayload()
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
            var msg = Message.FromPayload(AnyTypelessIBonded);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.RawPayload.Deserialize<SomePayloadType>();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayloadType>(deserializedPayload));
        }

        [Test]
        public void ConstructT_WithBonded_IsPayload()
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
            var msg = new Message(new Bonded<SomeDerivedPayloadType>(AnyDerivedPayload));
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.RawPayload.Deserialize<SomeDerivedPayloadType>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayloadType>(deserializedPayload));
        }

        [Test]
        public void ConstructT_WithDerivedPayload_IsPayload()
        {
            var msg = new Message<SomePayloadType>(AnyDerivedPayload);
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize<SomeDerivedPayloadType>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayloadType>(deserializedPayload));
        }

        [Test]
        public void FromPayload_WithDerivedPayload_IsPayload()
        {
            IBonded typelessBonded = new Bonded<SomeDerivedPayloadType>(AnyDerivedPayload);
            var msg = Message.FromPayload(typelessBonded);
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.RawPayload.Deserialize<SomeDerivedPayloadType>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayloadType>(deserializedPayload));
        }

        [Test]
        public void FromPayloadT_WithDerivedPayload_IsPayload()
        {
            var msg = Message.FromPayload<SomePayloadType>(AnyDerivedPayload);
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize<SomeDerivedPayloadType>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayloadType>(deserializedPayload));
        }

        [Test]
        public void ConstructT_WithError_IsPayload()
        {
            // There is no similar test for Message, as we have access to the
            // internal ctor, which overload resolution will pick. Normal users
            // of the library don't have access to this ctor and have to use
            // FromError. We have tests for FromError already.

            var msg = new Message<Error>(AnyError);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyError.IsEqual<Error>(deserializedPayload));
        }

        [Test]
        public void FromError_WithError_IsError()
        {
            var msg = Message.FromError(new Bonded<Error>(AnyError));

            Assert.IsTrue(msg.IsError);
            Assert.IsNotNull(msg.Error);
            Assert.IsTrue(AnyError.IsEqual<Error>(msg.Error.Deserialize()));

            Assert.Throws<InvalidOperationException>(() => { var _ = msg.RawPayload; });
        }

        [Test]
        public void FromErrorT_WithError_IsError()
        {
            var msg = Message.FromError<Error>(AnyError);

            Assert.IsTrue(msg.IsError);
            Assert.IsNotNull(msg.Error);
            Assert.IsTrue(AnyError.IsEqual<Error>(msg.Error.Deserialize()));

            Assert.Throws<InvalidOperationException>(() => { var _ = msg.Payload; });
        }

        [Test]
        public void FromError_ZeroErrorCode_Throws()
        {
            var zeroErrorCode = new Error {error_code = 0};
            Assert.Throws<ArgumentException>(() => Message.FromError<SomePayloadType>(zeroErrorCode));
            Assert.Throws<ArgumentException>(() => Message.FromError(zeroErrorCode));
        }

        [Test]
        public void Message_ConvertToMessageT_Works()
        {
            IBonded typelessBonded = new Bonded<SomeDerivedPayloadType>(AnyDerivedPayload);
            var msg = Message.FromPayload(typelessBonded);

            var messageBase = msg.Convert<SomePayloadType>();
            Assert.AreEqual(500, messageBase.Payload.Deserialize().int_field);

            var messageDerived = msg.Convert<SomeDerivedPayloadType>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayloadType>(messageDerived.Payload.Deserialize()));
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
