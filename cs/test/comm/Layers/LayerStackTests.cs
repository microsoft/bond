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
    using UnitTest.Interfaces;

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
            Assert.Throws<ArgumentException>(() => new LayerStack<Dummy>(new ILayer<Dummy>[0]));
            Assert.Throws<ArgumentNullException>(() => new LayerStack<Dummy>(null, null));
            Assert.Throws<ArgumentNullException>(() => new LayerStack<Dummy>(testLayer1, null));
        }

        [Test]
        public void LayerStack_OnSend_InvokesForwardOrder()
        {
            var testList = new List<string>();
            var testLayer1 = new TestLayer_Append("foo", testList);
            var testLayer2 = new TestLayer_Append("bar", testList);
            var stackProvider = new LayerStackProvider<Dummy>(testLayer1, testLayer2);
            Logger logger = LoggerTests.BlackHole;
            ILayerStack stack;
            Error error = stackProvider.GetLayerStack(null, out stack, logger);
            Assert.IsNull(error);

            IBonded layerData;
            error = stack.OnSend(MessageType.REQUEST, sendContext, out layerData, logger);

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
            var stackProvider = new LayerStackProvider<Dummy>(testLayer1, testLayer2);
            Logger logger = LoggerTests.BlackHole;
            ILayerStack stack;
            Error error = stackProvider.GetLayerStack(null, out stack, logger);
            Assert.IsNull(error);

            error = stack.OnReceive(MessageType.REQUEST, receiveContext, CreateBondedTestData(initialReceiveValue), logger);

            Assert.IsNull(error);
            Assert.AreEqual(2, testList.Count);
            Assert.AreEqual(initialReceiveValue + testLayer2.value, testList[0]);
            Assert.AreEqual(initialReceiveValue + testLayer2.value + testLayer1.value, testList[1]);
        }

        [Test]
        public void LayerStack_OnSend_ErrorOnThrow()
        {
            var stack = new LayerStack<Dummy>(new TestLayer_AlwaysThrows());

            Logger logger = LoggerTests.BlackHole;
            IBonded layerData;
            Error error = stack.OnSend(MessageType.REQUEST, sendContext, out layerData, logger);
            Assert.IsNotNull(error);
            Assert.AreEqual((int)ErrorCode.INTERNAL_SERVER_ERROR, error.error_code);
        }

        [Test]
        public void LayerStack_OnReceive_ErrorOnThrow()
        {
            var stack = new LayerStack<Dummy>(new TestLayer_AlwaysThrows());
            Logger logger = LoggerTests.BlackHole;
            Error error = stack.OnReceive(MessageType.REQUEST, receiveContext, CreateBondedTestData(initialReceiveValue), logger);
            Assert.IsNotNull(error);
            Assert.AreEqual((int)ErrorCode.INTERNAL_SERVER_ERROR, error.error_code);
        }

        public void LayerStackProvider_BadCtorArguments_Throw()
        {
            var testLayer1 = new TestLayer_AlwaysThrows();

            Assert.Throws<ArgumentException>(() => new LayerStackProvider<Dummy>(null));
            Assert.Throws<ArgumentException>(() => new LayerStackProvider<Dummy>(new ILayerProvider<Dummy>[0]));
            Assert.Throws<ArgumentNullException>(() => new LayerStackProvider<Dummy>(null, null));
            Assert.Throws<ArgumentNullException>(() => new LayerStackProvider<Dummy>(testLayer1, null));
        }


        [Test]
        public void LayerStackProvider_StatelessLayerStackDoesNotReallocate()
        {
            var provider = new LayerStackProvider<Dummy>(new TestLayer_AlwaysThrows(), new TestLayer_AlwaysThrows());
            Logger logger = LoggerTests.BlackHole;
            ILayerStack stack;
            Error error = provider.GetLayerStack(null, out stack, logger);
            Assert.IsNull(error);
            ILayerStack stack2;
            error = provider.GetLayerStack(null, out stack2, logger);
            Assert.IsNull(error);
            Assert.AreSame(stack, stack2);
        }

        [Test]
        public void LayerStackProvider_StatefulLayerStackDoesReallocate()
        {
            var provider = new LayerStackProvider<Dummy>(new TestLayer_AlwaysThrows(),
                                                         new TestLayerProvider_StatefulAppend("foo"),
                                                         new TestLayer_AlwaysThrows());
            Logger logger = LoggerTests.BlackHole;
            ILayerStack stack;
            Error error = provider.GetLayerStack(null, out stack, logger);
            Assert.IsNull(error);
            ILayerStack stack2;
            error = provider.GetLayerStack(null, out stack2, logger);
            Assert.IsNull(error);
            Assert.AreNotSame(stack, stack2);
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
        public TestSendContext() : base(new ConnectionMetrics(), new RequestMetrics()) { }

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
        public TestReceiveContext() : base(new ConnectionMetrics(), new RequestMetrics()) { }

        public override Connection Connection
        {
            get
            {
                throw new NotImplementedException();
            }
        }
    }

    // Not technically stateless but marked as such for testing purposes.
    public class TestLayer_Append : IStatelessLayer<Dummy>, ILayerProvider<Dummy>
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

        public ILayer<Dummy> GetLayer()
        {
            return this;
        }

        public Error OnSend(MessageType messageType, SendContext context, Dummy layerData, Logger logger)
        {
            layerData.string_value += value;
            list.Add(layerData.string_value);
            return null;
        }

        public Error OnReceive(MessageType messageType, ReceiveContext context, Dummy layerData, Logger logger)
        {
            layerData.string_value += value;
            list.Add(layerData.string_value);
            return null;
        }
    }

    public class TestLayer_AlwaysThrows : IStatelessLayer<Dummy>, ILayerProvider<Dummy>
    {
        public ILayer<Dummy> GetLayer()
        {
            return this;
        }

        public Error OnSend(MessageType messageType, SendContext context, Dummy layerData, Logger logger)
        {
            throw new LayerStackException();
        }

        public Error OnReceive(MessageType messageType, ReceiveContext context, Dummy layerData, Logger logger)
        {
            throw new LayerStackException();
        }

        private class LayerStackException : Exception
        {

        }
    }

    // Not technically stateless but marked as such for testing purposes.
    public class TestLayer_CheckPassedValue : IStatelessLayer<Dummy>, ILayerProvider<Dummy>
    {
        readonly int expectedValue;

        public TestLayer_CheckPassedValue(int value)
        {
            expectedValue = value;
        }

        public ILayer<Dummy> GetLayer()
        {
            return this;
        }

        public Error OnSend(MessageType messageType, SendContext context, Dummy layerData, Logger logger)
        {
            layerData.int_value = expectedValue;
            return null;
        }

        public Error OnReceive(MessageType messageType, ReceiveContext context, Dummy layerData, Logger logger)
        {
            if (layerData.int_value != expectedValue)
            {
                throw new ArgumentException(string.Format("Bad layer data: expected {0}, got {1}",
                                                          expectedValue, layerData.int_value));
            }
            return null;
        }
    }

    // Not technically stateless but marked as such for testing purposes.
    public class TestLayer_ReturnErrors : IStatelessLayer<Dummy>, ILayerProvider<Dummy>
    {
        public const int SendError = 0x0001234;
        public const int ReceiveError = 0x0005678;

        MessageType badMessageType;
        bool errorOnSend;
        bool errorOnReceive;

        public ILayer<Dummy> GetLayer()
        {
            return this;
        }

        public void SetState(MessageType badMessageType, bool errorOnSend, bool errorOnReceive)
        {
            this.badMessageType = badMessageType;
            this.errorOnSend = errorOnSend;
            this.errorOnReceive = errorOnReceive;
        }

        public Error OnSend(MessageType messageType, SendContext context, Dummy layerData, Logger logger)
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

        public Error OnReceive(MessageType messageType, ReceiveContext context, Dummy layerData, Logger logger)
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

    public class TestLayerProvider_StatefulAppend : ILayerProvider<Dummy>
    {
        public readonly string Prefix;

        public readonly List<TestLayer_StatefulAppend> Layers;

        public TestLayerProvider_StatefulAppend(string prefix)
        {
            Prefix = prefix;
            Layers = new List<TestLayer_StatefulAppend>();
        }

        public ILayer<Dummy> GetLayer()
        {
            var result = new TestLayer_StatefulAppend(Prefix + Layers.Count);
            Layers.Add(result);
            return result;
        }
    }

    public class TestLayer_StatefulAppend : ILayer<Dummy>
    {
        const string SendMessage = "Send";
        const string ReceiveMessage = "Receive";
        readonly string prefix;
        public string State { get; private set; }

        public TestLayer_StatefulAppend(string prefix)
        {
            this.prefix = prefix;
        }

        public Error OnSend(MessageType messageType, SendContext context, Dummy layerData, Logger logger)
        {
            State += prefix + SendMessage;
            return null;
        }

        public Error OnReceive(MessageType messageType, ReceiveContext context, Dummy layerData, Logger logger)
        {
            State += prefix + ReceiveMessage;
            return null;
        }
    }

    public class TestLayerStackProvider_Fails : ILayerStackProvider
    {
        public static string InternalDetails = "Internal details";
        uint failAfterGets;

        public TestLayerStackProvider_Fails(uint failAfterGets)
        {
            this.failAfterGets = failAfterGets;
        }

        public Error GetLayerStack(string uniqueId, out ILayerStack layerStack, Logger logger)
        {
            if (failAfterGets == 0)
            {
                layerStack = null;
                return Errors.MakeInternalServerError(InternalDetails, uniqueId);
            }
            else
            {
                failAfterGets--;
                layerStack = new LayerStack<Dummy>(new TestLayer_StatefulAppend("foo"));
                return null;
            }
        }
    }
}
