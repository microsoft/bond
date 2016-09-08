// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using System;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using NUnit.Framework;
    using UnitTest.Interfaces;

    [TestFixture]
    public class ResponseMapTests
    {
        static readonly IMessage anyResponse = Message.FromPayload(new SomePayload());

        [Test]
        public async Task ResponseMap_AddComplete_ReturnedTaskIsCompleted()
        {
            var respMap = new ResponseMap();
            Task<IMessage> responseTask = respMap.Add(1);
            Assert.AreEqual(1, respMap.OutstandingCount);

            bool wasCompleted = respMap.Complete(1, anyResponse);

            Assert.IsTrue(wasCompleted);
            Assert.AreEqual(0, respMap.OutstandingCount);
            var response = await responseTask;
            Assert.AreSame(anyResponse, response);
        }

        [Test]
        public void ResponseMap_AddTwice_Throws()
        {
            var respMap = new ResponseMap();
            respMap.Add(1);

            Assert.Throws<ArgumentException>(() => respMap.Add(1));
        }

        [Test]
        public async Task ResponseMap_CompleteTwice_OnlyFirstRegistered()
        {
            var respMap = new ResponseMap();
            Task<IMessage> responseTask = respMap.Add(1);

            bool wasCompleted = respMap.Complete(1, anyResponse);
            Assert.IsTrue(wasCompleted);
            IMessage anotherResponse = Message.FromPayload(new SomePayload());
            wasCompleted = respMap.Complete(1, anotherResponse);
            Assert.IsFalse(wasCompleted);

            var response = await responseTask;
            Assert.AreSame(anyResponse, response);
        }

        [Test]
        public async Task ResponseMap_Shutdown_SubsequentAddHasErrorResult()
        {
            var respMap = new ResponseMap();
            respMap.Shutdown();

            Task<IMessage> responseTask = respMap.Add(1);

            Assert.AreEqual(0, respMap.OutstandingCount);
            IMessage response = await responseTask;
            Assert.IsTrue(response.IsError);
            Error err = response.Error.Deserialize();
            Assert.AreEqual((int)ErrorCode.CONNECTION_SHUT_DOWN, err.error_code);
        }

        [Test]
        public async Task ResponseMap_Shutdown_PendingResponsesCompletedWithError()
        {
            var respMap = new ResponseMap();
            Task<IMessage> responseTask = respMap.Add(1);

            respMap.Shutdown();
            respMap.Complete(1, anyResponse);

            Assert.AreEqual(0, respMap.OutstandingCount);

            var response = await responseTask;
            Assert.IsTrue(response.IsError);
            Error err = response.Error.Deserialize();
            Assert.AreEqual((int)ErrorCode.CONNECTION_SHUT_DOWN, err.error_code);
        }

        [Test]
        public async Task ResponseMap_TakeWorks()
        {
            var respMap = new ResponseMap();
            Task<IMessage> responseTask = respMap.Add(1);
            Task<IMessage> otherTask = respMap.Add(2);

            Assert.AreEqual(2, respMap.OutstandingCount);

            TaskCompletionSource<IMessage> tcs = respMap.TakeTaskCompletionSource(1);
            Assert.AreSame(responseTask, tcs.Task);
            Assert.AreNotSame(otherTask, tcs.Task);

            Assert.AreEqual(1, respMap.OutstandingCount);

            tcs.SetResult(anyResponse);
            var response = await responseTask;
            Assert.AreSame(anyResponse, response);
        }

        [Test]
        public void ResponseMap_TakeAfterCompleteIsNull()
        {
            var respMap = new ResponseMap();
            respMap.Add(1);

            respMap.Complete(1, anyResponse);
            TaskCompletionSource<IMessage> tcs = respMap.TakeTaskCompletionSource(1);

            Assert.IsNull(tcs);
        }

        [Test]
        public void ResponseMap_CompleteAfterTakeIsFalse()
        {
            var respMap = new ResponseMap();
            respMap.Add(1);

            respMap.TakeTaskCompletionSource(1);
            bool result = respMap.Complete(1, anyResponse);

            Assert.IsFalse(result);
        }
    }
}
