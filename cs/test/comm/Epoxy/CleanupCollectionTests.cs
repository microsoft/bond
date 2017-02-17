// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using System;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using NUnit.Framework;

    [TestFixture]
    public class CleanupCollectionTests
    {
        [Test]
        public async Task EmptyCollection_CanBeShutdown()
        {
            var c = new CleanupCollection<object>();
            await c.CleanupAsync();
        }

        [Test]
        public async Task EmptyCollection_CanBeShutdownTwice()
        {
            var c = new CleanupCollection<object>();
            await c.CleanupAsync();
            await c.CleanupAsync();
        }

        [Test]
        public async Task NonEmptyCollection_NullCleanupFunc_CanBeShutdown()
        {
            var c = new CleanupCollection<object>();
            c.Add(new object());
            await c.CleanupAsync();
        }

        [Test]
        public async Task AfterShutdown_CanStillRemove()
        {
            var c = new CleanupCollection<object>();
            var obj1 = new object();

            c.Add(obj1);
            await c.CleanupAsync();

            c.Remove(obj1);
        }

        [Test]
        public async Task Shutdown_InvokesPassedCleanupFunc()
        {
            var c = new CleanupCollection<ShutdownRecorder>();
            Func<ShutdownRecorder, Task> cleanupFunc = sr =>
            {
                sr.Shutdown();
                return CodegenHelpers.CompletedTask;
            };

            var obj1 = new ShutdownRecorder();
            var obj2 = new ShutdownRecorder();
            var obj3 = new ShutdownRecorder();

            Assert.AreEqual(0, obj1.NumTimesShutdown);
            Assert.AreEqual(0, obj2.NumTimesShutdown);
            Assert.AreEqual(0, obj3.NumTimesShutdown);

            c.Add(obj1);
            c.Add(obj2);

            c.Add(obj3);
            c.Remove(obj3);

            await c.CleanupAsync(cleanupFunc);

            Assert.AreEqual(1, obj1.NumTimesShutdown);
            Assert.AreEqual(1, obj2.NumTimesShutdown);
            Assert.AreEqual(0, obj3.NumTimesShutdown);

            await c.CleanupAsync(cleanupFunc);

            Assert.AreEqual(1, obj1.NumTimesShutdown);
            Assert.AreEqual(1, obj2.NumTimesShutdown);
            Assert.AreEqual(0, obj3.NumTimesShutdown);
        }

        class ShutdownRecorder
        {
            public int NumTimesShutdown;

            public void Shutdown()
            {
                NumTimesShutdown += 1;
            }
        }
    }
}
