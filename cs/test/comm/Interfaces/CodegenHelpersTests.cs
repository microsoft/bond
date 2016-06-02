// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Interfaces
{
    using System;
    using System.Threading.Tasks;
    using Bond.Comm;
    using NUnit.Framework;

    [TestFixture]
    public class CodegenHelpersTests
    {
        [Test]
        public void Upcast_ConvertsDerivedToBase()
        {
            var derived = new Derived();
            var taskDerived = Task.FromResult(derived);
            Task<Base> taskBase = taskDerived.Upcast<Derived, Base>();

            Assert.IsTrue(taskBase.IsCompleted);
            Assert.IsFalse(taskBase.IsFaulted);
            Assert.IsFalse(taskBase.IsCanceled);

            Assert.AreSame(derived, taskBase.Result);
        }

        [Test]
        public void Upcast_PropagatesCancellation()
        {
            var tcsDerived = new TaskCompletionSource<Derived>();
            tcsDerived.SetCanceled();

            Task<Base> taskBase = tcsDerived.Task.Upcast<Derived, Base>();
            Assert.IsTrue(taskBase.IsCompleted);
            Assert.IsTrue(taskBase.IsCanceled);
        }

        [Test]
        public void Upcast_PropagatesException()
        {
            var exception = new Exception();
            var tcsDerived = new TaskCompletionSource<Derived>();
            tcsDerived.SetException(exception);

            Task<Base> taskBase = tcsDerived.Task.Upcast<Derived, Base>();
            Assert.IsTrue(taskBase.IsCompleted);
            Assert.IsTrue(taskBase.IsFaulted);

            Assert.AreSame(exception, taskBase.Exception.InnerException);
        }

        private class Base { }
        private class Derived : Base { }
    }
}
