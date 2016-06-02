// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Layers
{
    using System;
    using System.Collections.Generic;
    using Bond;
    using Bond.Comm;
    using Bond.Comm.Layers;
    using Bond.IO.Safe;
    using Bond.Protocols;
    using NUnit.Framework;
    using UnitTest.Comm;

    [TestFixture]
    public class LayerStackTests
    {
        static readonly TestSendContext sendContext = new TestSendContext();
        static readonly TestReceiveContext receiveContext = new TestReceiveContext();
        static readonly string initialReceiveValue = "baz";

        [Test]
        public void LayerStack_BadCtorArguments_Throw()
        {
            var testLayer1 = new TestLayer_AlwaysThrows();

            Assert.Throws<ArgumentException>(() => new LayerStack<Dummy>(null));
            Assert.Throws<ArgumentNullException>(() => new LayerStack<Dummy>(null, null));
            Assert.Throws<ArgumentNullException>(() => new LayerStack<Dummy>(testLayer1, null));
        }

        [Test]
        public void LayerStack_OnSend_InvokesForwardOrder()
        {
            var testList = new List<string>();
            var testLayer1 = new TestLayer_Append("foo", testList);
            var testLayer2 = new TestLayer_Append("bar", testList);
            var stack = new LayerStack<Dummy>(testLayer1, testLayer2);

            IBonded layerData;
            Error error = stack.OnSend(MessageType.Request, sendContext, out layerData);

            Assert.IsNull(error);
            Assert.IsNotNull(layerData);

            Dummy realLayerData = layerData.Deserialize<Dummy>();

            Assert.AreEqual(2, testList.Count);
            Assert.AreEqual(testLayer1.value, testList[0]);
            Assert.AreEqual(testLayer1.value + testLayer2.value, testList[1]);
            Assert.IsNotNull(realLayerData);
            Assert.AreEqual(testLayer1.value + testLayer2.value, realLayerData.string_value);
        }

        [Test]
        public void LayerStack_OnReceive_InvokesReverseOrder()
        {
            var testList = new List<string>();
            var testLayer1 = new TestLayer_Append("foo", testList);
            var testLayer2 = new TestLayer_Append("bar", testList);
            var stack = new LayerStack<Dummy>(testLayer1, testLayer2);

            Error error = stack.OnReceive(MessageType.Request, receiveContext, CreateBondedTestData(initialReceiveValue));

            Assert.IsNull(error);
            Assert.AreEqual(2, testList.Count);
            Assert.AreEqual(initialReceiveValue + testLayer2.value, testList[0]);
            Assert.AreEqual(initialReceiveValue + testLayer2.value + testLayer1.value, testList[1]);
        }

        [Test]
        public void LayerStack_OnSend_ErrorOnThrow()
        {
            var stack = new LayerStack<Dummy>(new TestLayer_AlwaysThrows());

            IBonded layerData;
            Error error = stack.OnSend(MessageType.Request, sendContext, out layerData);
            Assert.IsNotNull(error);
            Assert.AreEqual((int)ErrorCode.UnhandledLayerError, error.error_code);
        }

        [Test]
        public void LayerStack_OnReceive_ErrorOnThrow()
        {
            var stack = new LayerStack<Dummy>(new TestLayer_AlwaysThrows());

            Error error = stack.OnReceive(MessageType.Request, receiveContext, CreateBondedTestData(initialReceiveValue));
            Assert.IsNotNull(error);
            Assert.AreEqual((int)ErrorCode.UnhandledLayerError, error.error_code);
        }

        IBonded CreateBondedTestData(string value)
        {
            var realLayerData = new Dummy { string_value = value };

            var outputBuffer = new OutputBuffer(20);
            var compactWriter = new CompactBinaryWriter<OutputBuffer>(outputBuffer);
            Marshal.To<CompactBinaryWriter<OutputBuffer>, Dummy>(compactWriter, realLayerData);
            return Unmarshal.From(outputBuffer.Data);
        }
    }

    class TestSendContext : SendContext
    {
        public override Connection Connection
        {
            get
            {
                throw new NotImplementedException();
            }
        }
    }

    class TestReceiveContext : ReceiveContext
    {
        public override Connection Connection
        {
            get
            {
                throw new NotImplementedException();
            }
        }
    }

    public class TestLayer_Append : ILayer<Dummy>
    {
        public readonly string value;
        readonly List<string> list;

        public TestLayer_Append(string value, List<string> list)
        {
            if (value == null) { throw new ArgumentNullException(nameof(value)); }
            if (list == null) { throw new ArgumentNullException(nameof(list)); }

            this.value = value;
            this.list = list;
        }

        public Error OnSend(MessageType messageType, SendContext context, Dummy layerData)
        {
            layerData.string_value += value;
            list.Add(layerData.string_value);
            return null;
        }

        public Error OnReceive(MessageType messageType, ReceiveContext context, Dummy layerData)
        {
            layerData.string_value += value;
            list.Add(layerData.string_value);
            return null;
        }
    }

    public class TestLayer_AlwaysThrows : ILayer<Dummy>
    {
        public Error OnSend(MessageType messageType, SendContext context, Dummy layerData)
        {
            throw new LayerStackException();
        }

        public Error OnReceive(MessageType messageType, ReceiveContext context, Dummy layerData)
        {
            throw new LayerStackException();
        }

        private class LayerStackException : Exception
        {

        }
    }
    public class TestLayer_CheckPassedValue : ILayer<Dummy>
    {
        readonly int expectedValue;

        public TestLayer_CheckPassedValue(int value)
        {
            expectedValue = value;
        }

        public Error OnSend(MessageType messageType, SendContext context, Dummy layerData)
        {
            layerData.int_value = expectedValue;
            return null;
        }

        public Error OnReceive(MessageType messageType, ReceiveContext context, Dummy layerData)
        {
            if (layerData.int_value != expectedValue)
            {
                throw new ArgumentException(string.Format("Bad layer data: expected {0}, got {1}",
                                                          expectedValue, layerData.int_value));
            }
            return null;
        }
    }

    public class TestLayer_ReturnErrors : ILayer<Dummy>
    {
        public const int SendError = 0x0001234;
        public const int ReceiveError = 0x0005678;

        MessageType badMessageType;
        bool errorOnSend;
        bool errorOnReceive;

        public void SetState(MessageType badMessageType, bool errorOnSend, bool errorOnReceive)
        {
            this.badMessageType = badMessageType;
            this.errorOnSend = errorOnSend;
            this.errorOnReceive = errorOnReceive;
        }

        public Error OnSend(MessageType messageType, SendContext context, Dummy layerData)
        {
            if (errorOnSend && messageType == badMessageType)
            {
                return new Error { error_code = SendError, message = string.Format("Send error {0}", messageType.ToString()) };
            }
            else
            {
                return null;
            }
        }

        public Error OnReceive(MessageType messageType, ReceiveContext context, Dummy layerData)
        {
            if (errorOnReceive && messageType == badMessageType)
            {
                return new Error { error_code = ReceiveError, message = string.Format("Receive error {0}", messageType.ToString()) };
            }
            else
            {
                return null;
            }
        }
    }
}
