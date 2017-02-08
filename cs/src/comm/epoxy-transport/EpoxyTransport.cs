// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Collections.Generic;
    using System.Net;
    using System.Net.Sockets;
    using System.Threading;
    using System.Threading.Tasks;

    public class EpoxyTransportBuilder : TransportBuilder<EpoxyTransport>
    {
        Func<string, Task<IPAddress>> resolver;
        EpoxyServerTlsConfig serverTlsConfig;
        EpoxyClientTlsConfig clientTlsConfig;

        EpoxyTransport.TimeoutConfig timeoutConfig;

        /// <summary>
        /// Sets a customer host resolver.
        /// </summary>
        /// <param name="resolver">
        /// The host resolver to use. May be <c>null</c>, in which case DNS
        /// resolution will be used.
        /// </param>
        /// <returns>The builder.</returns>
        /// <remarks>
        /// A resolver takes a string host name and turns it into one
        /// <see cref="IPAddress"/> however it sees fit.
        /// </remarks>
        public EpoxyTransportBuilder SetResolver(Func<string, Task<IPAddress>> resolver)
        {
            this.resolver = resolver;
            return this;
        }

        /// <summary>
        /// Sets the server-side TLS config to use when creating listeners.
        /// </summary>
        /// <param name="tlsConfig">
        /// The server-side config. May be <c>null</c>.
        /// </param>
        /// <returns>The builder.</returns>
        /// <remarks>
        /// If <c>null</c>, the listeners created will not use TLS to secure
        /// their communications with clients.
        /// </remarks>
        public EpoxyTransportBuilder SetServerTlsConfig(EpoxyServerTlsConfig tlsConfig)
        {
            serverTlsConfig = tlsConfig;
            return this;
        }

        /// <summary>
        /// Sets the client-side TLS config to use when establishing
        /// connections.
        /// </summary>
        /// <param name="tlsConfig">
        /// The client-side config. May be <c>null</c>.
        /// </param>
        /// <returns>The builder.</returns>
        /// <remarks>
        /// If <c>null</c>,
        /// <see cref="EpoxyClientTlsConfig.Default">EpoxyClientTlsConfig.Default</see>
        /// will be used for secure connections.
        /// </remarks>
        public EpoxyTransportBuilder SetClientTlsConfig(EpoxyClientTlsConfig tlsConfig)
        {
            clientTlsConfig = tlsConfig;
            return this;
        }

        /// <summary>
        /// Sets the transport's per-connection keep-alive timeout and interval.
        /// </summary>
        /// <param name="keepAliveTime">
        /// The amount of time to wait for the connection to be idle before starting to send
        /// keep-alive probes.
        /// </param>
        /// <param name="keepAliveInterval">
        /// The interval to wait between unsuccessful keep-alive probes.
        /// </param>
        /// <returns>The builder.</returns>
        /// <remarks>
        /// <para>
        /// Connection keep-alive can be used to detect if an Epoxy connection has gone down even if
        /// there are no messages being sent. If both keep-alive parameters are set to non-zero
        /// values, connection keep-alive will be enabled, otherwise no keep-alive probing will be
        /// performed and detection of a down connection will rely on attempts to send messages.
        /// </para>
        /// <para>
        /// When keep-alive is being used, some arbitrary, fixed number of probes must fail in a row
        /// before the connection is declared dead. The number of probes is not configurable.
        /// </para>
        /// <para>
        /// Connection keep-alive utilizes TCP keep-alive if available. If TCP keep-alive is not
        /// available or cannot be configured, these settings have no effect and detection of a down
        /// connection will rely on attempts to send messages.
        /// </para>
        /// </remarks>
        public EpoxyTransportBuilder SetKeepAliveTimes(
            TimeSpan keepAliveTime,
            TimeSpan keepAliveInterval)
        {
            timeoutConfig.KeepAliveTime = keepAliveTime;
            timeoutConfig.KeepAliveInterval = keepAliveInterval;
            return this;
        }

        public override EpoxyTransport Construct()
        {
            return new EpoxyTransport(
                LayerStackProvider,
                resolver,
                serverTlsConfig,
                clientTlsConfig,
                timeoutConfig,
                LogSink,
                EnableDebugLogs,
                MetricsSink);
        }
    }

    public class EpoxyTransport : Transport<EpoxyConnection, EpoxyListener>
    {
        public const int DefaultInsecurePort = 25188;
        public const int DefaultSecurePort = 25156;

        readonly ILayerStackProvider layerStackProvider;
        readonly Func<string, Task<IPAddress>> resolver;
        readonly EpoxyServerTlsConfig serverTlsConfig;
        readonly EpoxyClientTlsConfig clientTlsConfig;
        readonly TimeoutConfig timeoutConfig;
        readonly Logger logger;
        readonly Metrics metrics;

        readonly ReaderWriterLockSlim shutdownLock;
        bool isShutdown;

        readonly object collectionsLock;
        readonly HashSet<EpoxyConnection> connections;
        readonly HashSet<EpoxyListener> listeners;

        public struct Endpoint
        {
            public readonly string Host;
            public readonly int Port;
            public readonly bool UseTls;

            public Endpoint(string host, int port, bool useTls)
            {
                Host = host;
                Port = port;
                UseTls = useTls;
            }

            public Endpoint(IPEndPoint ipEndPoint, bool useTls)
                : this(
                      ipEndPoint.Address.ToString(),
                      ipEndPoint.Port,
                      useTls)
            {
            }

            public override string ToString()
            {
                string scheme = UseTls ? "epoxys" : "epoxy";
                return new UriBuilder(scheme, Host, Port).Uri.ToString();
            }
        }

        public struct TimeoutConfig
        {
            public TimeSpan KeepAliveTime;
            public TimeSpan KeepAliveInterval;
        }

        public EpoxyTransport(
            ILayerStackProvider layerStackProvider,
            Func<string, Task<IPAddress>> resolver,
            EpoxyServerTlsConfig serverTlsConfig,
            EpoxyClientTlsConfig clientTlsConfig,
            TimeoutConfig timeoutConfig,
            ILogSink logSink, bool enableDebugLogs,
            IMetricsSink metricsSink)
        {
            // Layer stack provider may be null
            this.layerStackProvider = layerStackProvider;

            // Always need a resolver, so substitute in default if given null
            this.resolver = resolver ?? ResolveViaDnsAsync;

            // may be null - indicates no TLS for listeners
            this.serverTlsConfig = serverTlsConfig;

            // Client-side TLS is determined by how the connection is
            // established, so we substitute in the Default TLS config if we
            // happened to get null
            this.clientTlsConfig = clientTlsConfig ?? EpoxyClientTlsConfig.Default;

            this.timeoutConfig = timeoutConfig;

            // Log sink may be null
            logger = new Logger(logSink, enableDebugLogs);
            // Metrics sink may be null
            metrics = new Metrics(metricsSink);

            shutdownLock = new ReaderWriterLockSlim(LockRecursionPolicy.NoRecursion);
            isShutdown = false;

            collectionsLock = new object();
            connections = new HashSet<EpoxyConnection>();
            listeners = new HashSet<EpoxyListener>();
        }

        public override Error GetLayerStack(string uniqueId, out ILayerStack stack)
        {
            if (layerStackProvider != null)
            {
                return layerStackProvider.GetLayerStack(uniqueId, out stack, logger);
            }
            else
            {
                stack = null;
                return null;
            }
        }

        public static Endpoint? Parse(string address, Logger logger)
        {
            Uri uri;
            try
            {
                uri = new Uri(address);
            }
            catch (Exception ex) when (ex is UriFormatException || ex is ArgumentNullException)
            {
                logger.Site().Error(ex, "given {0}, but URI parsing threw {1}", address, ex.Message);
                return null;
            }

            int schemeDefaultPort;
            bool useTls;

            switch (uri.Scheme)
            {
                case "epoxy":
                    schemeDefaultPort = DefaultInsecurePort;
                    useTls = false;
                    break;

                case "epoxys":
                    schemeDefaultPort = DefaultSecurePort;
                    useTls = true;
                    break;

                default:
                    logger.Site().Error("given {0}, but URI scheme must be epoxy:// or epoxys://", address);
                    return null;
            }

            var port = uri.Port != -1 ? uri.Port : schemeDefaultPort;

            if (uri.PathAndQuery != "/")
            {
                logger.Site().Error("given {0}, but Epoxy does not accept a path/resource", address);
                return null;
            }

            return new Endpoint(uri.DnsSafeHost, port, useTls);
        }

        /// <param name="address">A URI with a scheme of epoxy:// (insecure epoxy) or epoxys:// (epoxy over TLS).</param>
        /// <param name="ct">Unused.</param>
        public override Task<EpoxyConnection> ConnectToAsync(string address, CancellationToken ct)
        {
            var endpoint = Parse(address, logger);
            if (endpoint == null)
            {
                throw new ArgumentException(address + " was not a valid Epoxy URI", nameof(address));
            }

            return ConnectToAsync(endpoint.Value, ct);
        }

        /// <param name="address">A URI with a scheme of epoxy:// (insecure epoxy) or epoxys:// (epoxy over TLS).</param>
        public override Task<EpoxyConnection> ConnectToAsync(string address)
        {
            return ConnectToAsync(address, CancellationToken.None);
        }

        public async Task<EpoxyConnection> ConnectToAsync(Endpoint endpoint, CancellationToken ct)
        {
            logger.Site().Information("Connecting to {0}.", endpoint);

            EpoxyClientTlsConfig tlsConfig = endpoint.UseTls ? clientTlsConfig : null;

            try
            {
                EpoxyNetworkStream epoxyStream =
                    await EpoxyNetworkStream.MakeAsync(
                        socketFunc: () => ConnectClientSocketAsync(endpoint),
                        streamFunc: socket => EpoxyNetworkStream.MakeClientStreamAsync(endpoint.Host, socket, tlsConfig, logger),
                        timeoutConfig: timeoutConfig,
                        logger: logger);

                var connection = EpoxyConnection.MakeClientConnection(this, epoxyStream, logger, metrics);

                {
                    shutdownLock.EnterReadLock();
                    try
                    {
                        if (isShutdown)
                        {
                            throw new InvalidOperationException("This EpoxyTransport has been stopped already.");
                        }
                        lock (collectionsLock)
                        {
                            connections.Add(connection);
                        }
                    }
                    finally
                    {
                        shutdownLock.ExitReadLock();
                    }
                }

                await connection.StartAsync();
                return connection;
            }
            catch (Exception ex)
            {
                logger.Site().Error(ex, "Failed to start Epoxy client connection to '{0}'", endpoint);
                throw;
            }
        }

        public Task<EpoxyConnection> ConnectToAsync(Endpoint endpoint)
        {
            return ConnectToAsync(endpoint, CancellationToken.None);
        }

        public override EpoxyListener MakeListener(string address)
        {
            return MakeListener(ParseStringAddress(address));
        }

        public EpoxyListener MakeListener(IPEndPoint address)
        {
            var listener = new EpoxyListener(this, address, serverTlsConfig, timeoutConfig, logger, metrics);

            {
                shutdownLock.EnterReadLock();

                try
                {
                    if (isShutdown)
                    {
                        throw new InvalidOperationException(
                            "This EpoxyTransport has been stopped already.");
                    }

                    lock (collectionsLock)
                    {
                        listeners.Add(listener);
                    }
                }
                finally
                {
                    shutdownLock.ExitReadLock();
                }
            }

            return listener;
        }

        public override Task StopAsync()
        {
            try
            {
                shutdownLock.EnterWriteLock();

                if (isShutdown)
                {
                    return CodegenHelpers.CompletedTask;
                }

                isShutdown = true;
            }
            finally
            {
                shutdownLock.ExitWriteLock();
            }

            HashSet<EpoxyConnection> connectionsToStop;
            HashSet<EpoxyListener> listenersToStop;

            lock (collectionsLock)
            {
                connectionsToStop = new HashSet<EpoxyConnection>(connections);
                connections.Clear();
                listenersToStop = new HashSet<EpoxyListener>(listeners);
                listeners.Clear();
            }

            int totalStopTasks = checked(connectionsToStop.Count + listenersToStop.Count);
            var stopTasks = new Task[totalStopTasks];
            int idx = 0;
            foreach (var connection in connectionsToStop)
            {
                logger.Site().Debug("Stopping connection {0}", connection);
                stopTasks[idx] = connection.StopAsync();
                ++idx;
            }
            foreach (var listener in listenersToStop)
            {
                logger.Site().Debug("Stopping listener {0}", listener);
                stopTasks[idx] = listener.StopAsync();
                ++idx;
            }

            return Task.WhenAll(stopTasks);
        }

        public static IPEndPoint ParseStringAddress(string address)
        {
            if (string.IsNullOrEmpty(address))
            {
                throw new ArgumentException("Address cannot be null or empty", nameof(address));
            }

            int portStartIndex = address.IndexOf(':');

            string ipAddressPart;

            if (portStartIndex == -1)
            {
                ipAddressPart = address;
            }
            else
            {
                ipAddressPart = address.Substring(0, portStartIndex);
            }

            IPAddress ipAddr;
            if (!IPAddress.TryParse(ipAddressPart, out ipAddr))
            {
                throw new ArgumentException("Couldn't parse IP address from \"" + address + "\"", nameof(address));
            }

            int port;
            if (portStartIndex == -1)
            {
                port = DefaultInsecurePort;
            }
            else
            {
                string portPart = address.Substring(portStartIndex + 1);
                if (!int.TryParse(portPart, out port))
                {
                    throw new ArgumentException("Couldn't parse port from \"" + address + "\"", nameof(address));
                }
            }

            return new IPEndPoint(ipAddr, port);
        }

        private static async Task<IPAddress> ResolveViaDnsAsync(string host)
        {
            IPAddress[] ipAddresses;

            try
            {
                 ipAddresses = await Dns.GetHostAddressesAsync(host);
            }
            catch (SocketException ex)
            {
                throw new EpoxyFailedToResolveException($"Failed to resolve [{host}] due to DNS error", ex);
            }

            if (ipAddresses.Length < 1)
            {
                throw new EpoxyFailedToResolveException($"Could not resolve [{host}] to a supported IP address.");
            }

            return ipAddresses[0];
        }

        private async Task<Socket> ConnectClientSocketAsync(Endpoint endpoint)
        {
            logger.Site().Information("Resolving to {0}.", endpoint.Host);

            IPAddress ipAddress;

            try
            {
                ipAddress = await resolver(endpoint.Host);
            }
            catch (EpoxyFailedToResolveException ex)
            {
                logger.Site().Error(ex, "Failed to resolve {0}: {1}", endpoint, ex.Message);
                throw;
            }

            logger.Site().Debug("Resolved {0} to {1}.", endpoint.Host, ipAddress);

            var socket = new Socket(ipAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
            await Task.Factory.FromAsync(
                socket.BeginConnect,
                socket.EndConnect,
                ipAddress,
                endpoint.Port,
                state: null);

            logger.Site().Information(
                "Established TCP connection to {0} at {1}:{2}",
                endpoint.Host, ipAddress, endpoint.Port);
            return socket;
        }
    }
}
