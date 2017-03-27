// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Interfaces
{
    using System;
    using Bond;
    using Bond.Grpc;
    using NUnit.Framework;

    [TestFixture]
    class MessageTests
    {
        private static readonly SomePayload AnyPayload = new SomePayload
        {
            int_field = 100,
        };

        private static readonly SomeDerivedPayload AnyDerivedPayload = new SomeDerivedPayload
        {
            int_field = 500,
            bool_field = true,
        };

        [Test]
        public void Construct_Null_Throws()
        {
            Assert.Throws<ArgumentNullException>(() => new Message<SomePayload>((SomePayload)null));
            Assert.Throws<ArgumentNullException>(() => new Message<SomePayload>((IBonded<SomePayload>)null));
        }

        [Test]
        public void From_Null_Throws()
        {
            Assert.Throws<ArgumentNullException>(() => Message.From((SomePayload)null));
            Assert.Throws<ArgumentNullException>(() => Message.From((IBonded<SomePayload>)null));
        }

        [Test]
        public void Construct_CanDeserialize()
        {
            var msg = new Message<SomePayload>(AnyPayload);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayload>(deserializedPayload));
        }

        [Test]
        public void From_CanDeserialize()
        {
            var msg = Message.From(AnyPayload);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayload>(deserializedPayload));
        }

        [Test]
        public void Construct_WithBonded_CanDeserialize()
        {
            var bondedPayload = new Bonded<SomePayload>(AnyPayload);
            var msg = new Message<SomePayload>(bondedPayload);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayload>(deserializedPayload));
        }

        [Test]
        public void From_WithBonded_CanDeserialize()
        {
            IBonded<SomePayload> bondedPayload = new Bonded<SomePayload>(AnyPayload);
            var msg = Message.From(bondedPayload);

            var deserializedPayload = msg.Payload.Deserialize();
            Assert.IsTrue(AnyPayload.IsEqual<SomePayload>(deserializedPayload));
        }

        [Test]
        public void Construct_WithDerivedObj_DoesntSlice()
        {
            var msg = new Message<SomePayload>(AnyDerivedPayload);

            var deserializedPayload = msg.Payload.Deserialize<SomeDerivedPayload>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayload>(deserializedPayload));
        }

        [Test]
        public void From_WithDerivedObj_DoesntSlice()
        {
            var msg = Message.From<SomePayload>(AnyDerivedPayload);

            var deserializedPayload = msg.Payload.Deserialize<SomeDerivedPayload>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayload>(deserializedPayload));
        }

        [Test]
        public void From_WithDerivedBonded_DoesntSlice()
        {
            var msg = Message.From<SomePayload>(new Bonded<SomeDerivedPayload>(AnyDerivedPayload));

            var deserializedPayload = msg.Payload.Deserialize<SomeDerivedPayload>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayload>(deserializedPayload));
        }

        [Test]
        public void Convert_ToBase_Works()
        {
            IMessage<SomeDerivedPayload> msg = Message.From(AnyDerivedPayload);
            var msgBase = msg.Convert<SomePayload>();

            Assert.IsNotNull(msgBase);
            Assert.AreEqual(500, msgBase.Payload.Deserialize().int_field);
        }

        [Test]
        public void Convert_ToDerived_Works()
        {
            IMessage<SomePayload> msg = Message.From<SomePayload>(AnyDerivedPayload);
            var msgDerived = msg.Convert<SomeDerivedPayload>();

            Assert.IsNotNull(msgDerived);

            var deserializedPayload = msgDerived.Payload.Deserialize<SomeDerivedPayload>();
            Assert.IsTrue(AnyDerivedPayload.IsEqual<SomeDerivedPayload>(deserializedPayload));
        }

        [Test]
        public void Convert_BadConversion_ReturnsNull()
        {
            var msg = Message.From(AnyPayload);
            Assert.IsNull(msg.Convert<Bond.Void>());
        }
    }
}
