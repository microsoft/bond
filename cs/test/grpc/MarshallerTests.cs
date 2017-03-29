// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Grpc
{
    using Bond.Grpc;
    using NUnit.Framework;

    [TestFixture]
    class MarshallerTests
    {
        [Test]
        public void Marshaller_Serializer_HandlesNull()
        {
            Assert.IsNull(Marshaller<Bond.Void>.Instance.Serializer(null));
        }

        [Test]
        public void Marshaller_Deserializer_HandlesNull()
        {
            Assert.IsNull(Marshaller<Bond.Void>.Instance.Deserializer(null));
        }

        [Test]
        public void Marshaller_CanRoundTrip()
        {
            var somePayload = new SomePayload { int_field = 100 };

            var grpcMarshalledData = Marshaller<SomePayload>.Instance.Serializer(Message.From(somePayload));
            IMessage<SomePayload> unmarshalledData =
                Marshaller<SomePayload>.Instance.Deserializer(grpcMarshalledData);
            SomePayload deserializedData = unmarshalledData.Payload.Deserialize();

            Assert.IsTrue(somePayload.IsEqual<SomePayload>(deserializedData));
        }

        [Test]
        public void Marshaller_DerivedDataSurvives()
        {
            var someDerivedPayload = new SomeDerivedPayload { int_field = 100, bool_field = true};

            var grpcMarshalledData = Marshaller<SomePayload>.Instance.Serializer(Message.From(someDerivedPayload));
            IMessage<SomePayload> unmarshalledData =
                Marshaller<SomePayload>.Instance.Deserializer(grpcMarshalledData);
            SomeDerivedPayload deserializedData = unmarshalledData.Payload.Deserialize<SomeDerivedPayload>();

            Assert.IsTrue(someDerivedPayload.IsEqual<SomeDerivedPayload>(deserializedData));
        }
    }
}
