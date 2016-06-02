// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Interfaces
{
    using System;
    using Bond.Comm;
    using NUnit.Framework;

    [TestFixture]
    class TransportTests
    {
        [Test]
        public void MakeInternalServerError_DontIncludeDetails_GenericErrorReturned()
        {
            var ex = new InvalidOperationException("You can't do that.");

            InternalServerError error = Transport.MakeInternalServerError(ex, includeDetails: false);

            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);
            Assert.IsNotEmpty(error.unique_id);
            Assert.That(error.message, Is.Not.StringContaining(ex.Message));
            Assert.IsEmpty(error.server_stack_trace);
            Assert.IsEmpty(error.inner_errors);
        }

        [Test]
        public void MakeInternalServerError_NullExIncludeDetails_GenericErrorReturned()
        {
            InternalServerError error = Transport.MakeInternalServerError(exception: null, includeDetails: true);
            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);
            Assert.IsNotEmpty(error.unique_id);
            Assert.IsNotEmpty(error.message);
            Assert.IsEmpty(error.server_stack_trace);
            Assert.IsEmpty(error.inner_errors);
        }

        [Test]
        public void MakeInternalServerError_ExIncludeDetails_HasStackAndInnerExceptions()
        {
            var ex = GenerateException(new Exception("this is some message", GenerateException<InvalidOperationException>()));
            InternalServerError error = Transport.MakeInternalServerError(ex, includeDetails: true);

            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);
            Assert.IsNotEmpty(error.unique_id);
            Assert.That(error.message, Is.StringContaining(ex.Message));
            Assert.IsNotEmpty(error.server_stack_trace);
            Assert.AreEqual(1, error.inner_errors.Count);
        }

        [Test]
        public void MakeInternalServerError_AggExIncludeDetails_HasStackAndInnerExceptions()
        {
            var innerExceptions = new[] { GenerateException<ArgumentException>(), GenerateException<InvalidOperationException>() };
            var aggEx = GenerateException(new AggregateException("this is some message", innerExceptions));
            InternalServerError error = Transport.MakeInternalServerError(aggEx, includeDetails: true);

            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);
            Assert.IsNotEmpty(error.unique_id);
            Assert.That(error.message, Is.StringContaining(aggEx.Message));
            Assert.IsNotEmpty(error.server_stack_trace);
            Assert.AreEqual(2, error.inner_errors.Count);
        }

        private static Exception GenerateException(Exception ex)
        {
            try
            {
                throw ex;
            }
            catch (Exception exCaught)
            {
                return exCaught;
            }
        }

        private static Exception GenerateException<TException>() where TException : Exception, new()
        {
            return GenerateException(new TException());
        }
    }
}
