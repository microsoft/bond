// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System;
    using System.Threading.Tasks;

    public class SimpleInMemListener : Listener
    {
        private SimpleInMemServiceHost m_serviceHost;
        private string m_address;
        private SimpleInMemConnection m_connection;
        private readonly string m_logname;

        public SimpleInMemListener(SimpleInMemTransport parentTransport, string address)
        {
            m_address = address;
            m_serviceHost = new SimpleInMemServiceHost(parentTransport);
            m_connection = new SimpleInMemConnection(m_serviceHost, ConnectionType.Server);
            m_logname = $"{nameof(SimpleInMemListener)}({m_address})";
        }

        public override void AddService<T>(T service)
        {
            Log.Information("{0}.{1}: Adding {2}.", m_logname, nameof(AddService), typeof(T).Name);
            m_serviceHost.Register(service);
        }

        public override void RemoveService<T>(T service)
        {
            throw new NotImplementedException();
        }

        public override Task StartAsync()
        {
            m_connection.Start();
            return TaskExt.CompletedTask;
        }

        public override Task StopAsync()
        {
            m_connection.StopAsync();
            return TaskExt.CompletedTask;
        }

        internal SimpleInMemConnection Connection
        {
            get
            {
                return m_connection;
            }
        }
    }
}
