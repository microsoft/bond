// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem.Processor
{
    using System;
    using System.Threading;
    using System.Threading.Tasks.Dataflow;
    using Bond.Comm.Service;

    internal abstract class QueueProcessor
    {
        private const int MIN_DELAY_MILLISEC = 1;
        private const int MAX_DELAY_MILLISEC = 11;
        private readonly object processInput = new object();
        protected readonly SimpleInMemConnection connection;
        protected readonly ServiceHost serviceHost;
        protected readonly SimpleInMemTransport transport;
        protected readonly Logger logger;

        internal QueueProcessor(SimpleInMemConnection connection, ServiceHost serviceHost,
            SimpleInMemTransport transport, Logger logger)
        {
            if (connection == null) throw new ArgumentNullException(nameof(connection));
            if (serviceHost == null) throw new ArgumentNullException(nameof(serviceHost));
            if (transport == null) throw new ArgumentNullException(nameof(transport));

            this.connection = connection;
            this.serviceHost = serviceHost;
            this.transport = transport;
            this.logger = logger;
        }

        /// <summary>
        /// Asynchronously calls <see cref="Process()"/> method
        /// until <see cref="CancellationTokenSource.Cancel()"/> is called.
        /// </summary>
        public void ProcessAsync(CancellationToken t)
        {
            Util.CreateLongRunningTask(o => Process(), t, MIN_DELAY_MILLISEC, MAX_DELAY_MILLISEC).Post(processInput);
        }

        /// <summary>
        /// Process instances of <see cref="InMemFrame"/> that are enqueued on
        /// <see cref="SimpleInMemConnection.ReadQueue"/>. Response generated during
        /// processing is written onto <see cref="SimpleInMemConnection.WriteQueue"/>.
        /// </summary>
        internal abstract void Process();
    }
}
