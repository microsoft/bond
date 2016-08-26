// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Interfaces
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

        private static readonly SomePayload AnyPayload = new SomePayload
        {
            int_field = 100,
        };

        private static readonly IBonded AnyTypelessIBonded = (IBonded)new Bonded<SomePayload>(AnyPayload);

        private static readonly SomeDerivedPayload AnyDerivedPayload = new SomeDerivedPayload
        {
            int_field = 500,
            bool_field = true,
        };

        [Test]
        public void Construct_Null_Throws()
        {
            Assert.Throws<ArgumentNullException>(() => new Message((IBonded) null));
            Assert.Throws<ArgumentNullException>(() => new Message((IBonded<Error>)null));

            Assert.Throws<ArgumentNullException>(() => new Message<SomePayload>((SomePayload)null));
            Assert.Throws<ArgumentNullException>(() => new Message<SomePayload>((IBonded<SomePayload>)null));
            Assert.Throws<ArgumentNullException>(() => new Message<SomePayload>((IBonded<Error>)null));
        }

        [Test]
        public void Construct_WithPayload_IsPayload()
        {
            var msg = new Message(AnyTypelessIBonded);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.RawPayload.Deserialize<SomePayload>();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayload>(deserializedPayload));
        }

        [Test]
        public void ConstructT_WithPayload_IsPayload()
        {
            var msg = new Message<SomePayload>(AnyPayload);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayload>(deserializedPayload));
        }

        [Test]
        public void FromPayload_WithPayload_IsPayload()
        {
            var msg = Message.FromPayload(AnyTypelessIBonded);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.RawPayload.Deserialize<SomePayload>();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayload>(deserializedPayload));
        }

        [Test]
        public void FromPayload_BondedNull_Throws()
        {
            Assert.Throws<ArgumentNullException>(() => Message.FromPayload((SomePayload) null));
            Assert.Throws<ArgumentNullException>(() => Message.FromPayload((IBonded) null));
            Assert.Throws<ArgumentNullException>(() => Message.FromPayload((IBonded<SomePayload>)null));
        }

        [Test]
        public void FromError_Null_Throws()
        {
            Assert.Throws<ArgumentNullException>(() => Message.FromError((Error) null));
            Assert.Throws<ArgumentNullException>(() => Message.FromError((IBonded<Error>)null));
            Assert.Throws<ArgumentNullException>(() => Message.FromError<SomePayload>((Error)null));
            Assert.Throws<ArgumentNullException>(() => Message.FromError<SomePayload>((IBonded<Error>)null));
        }

        [Test]
        public void FromPayloadT_WithPayload_IsPayload()
        {
            var msg = Message.FromPayload(AnyPayload);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayload>(deserializedPayload));
        }

        [Test]
        public void Construct_WithBonded_IsPayload()
        {
            var msg = Message.FromPayload(AnyTypelessIBonded);

            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.RawPayload.Deserialize<SomePayload>();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayload>(deserializedPayload));
        }

        [Test]
        public void ConstructT_WithBonded_IsPayload()
        {
            var bondedPayload = new Bonded<SomePayload>(AnyPayload);
            var msg = new Message<SomePayload>(bondedPayload);
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayload>(deserializedPayload));
        }

        [Test]
        public void Construct_WithDerivedPayload_IsPayload()
        {
            var msg = new Message(new Bonded<SomeDerivedPayload>(AnyDerivedPayload));
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.RawPayload.Deserialize<SomeDerivedPayload>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayload>(deserializedPayload));
        }

        [Test]
        public void ConstructT_WithDerivedPayload_IsPayload()
        {
            var msg = new Message<SomePayload>(AnyDerivedPayload);
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize<SomeDerivedPayload>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayload>(deserializedPayload));
        }

        [Test]
        public void FromPayload_WithDerivedPayload_IsPayload()
        {
            IBonded typelessBonded = new Bonded<SomeDerivedPayload>(AnyDerivedPayload);
            var msg = Message.FromPayload(typelessBonded);
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.RawPayload.Deserialize<SomeDerivedPayload>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayload>(deserializedPayload));
        }

        [Test]
        public void FromPayloadT_WithDerivedPayload_IsPayload()
        {
            var msg = Message.FromPayload<SomePayload>(AnyDerivedPayload);
            Assert.IsFalse(msg.IsError);
            Assert.IsNull(msg.Error);

            var deserializedPayload = msg.Payload.Deserialize<SomeDerivedPayload>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayload>(deserializedPayload));
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
        public void Message_ConvertToMessageT_Works()
        {
            IBonded typelessBonded = new Bonded<SomeDerivedPayload>(AnyDerivedPayload);
            var msg = Message.FromPayload(typelessBonded);

            var messageBase = msg.Convert<SomePayload>();
            Assert.AreEqual(500, messageBase.Payload.Deserialize().int_field);

            var messageDerived = msg.Convert<SomeDerivedPayload>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayload>(messageDerived.Payload.Deserialize()));
        }
    }
}
