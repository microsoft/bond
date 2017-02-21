// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Diagnostics;
    using System.IO;
    using System.Net;
    using System.Net.Security;
    using System.Net.Sockets;
    using System.Security.Authentication;
    using System.Security.Cryptography.X509Certificates;
    using System.Threading;
    using System.Threading.Tasks;

    /// <summary>
    /// Epoxy-private wrapper around <see cref="Socket"/>. Provides idempotent shutdown.
    /// </summary>
    internal class EpoxyNetworkStream
    {
        const SslProtocols AllowedTlsProtocols = SslProtocols.Tls12;

        Socket socket;
        Stream stream;

        // Caches of the values from the underlying socket so we can use them after socket
        // shutdown.
        public IPEndPoint LocalEndpoint { get; }
        public IPEndPoint RemoteEndpoint { get; }

        // It looks like we don't need to .Dispose this SemaphoreSlim. The
        // current implementation of SemaphoreSlim only does interesting
        // stuff during .Dispose if there's an allocated
        // AvailableWaitHandle. We never call that, so there shouldn't be
        // anything needing disposal. If we do end up allocating a wait
        // handle somehow, its finalizer will save us.
        public SemaphoreSlim WriteLock { get; }

        int isShutdown;
        Logger logger;

        private EpoxyNetworkStream(Socket socket, Stream stream, Logger logger)
        {
            Debug.Assert(socket != null);
            Debug.Assert(stream != null);
            Debug.Assert(logger != null);

            this.socket = socket;
            this.stream = stream;

            LocalEndpoint = (IPEndPoint)socket.LocalEndPoint;
            RemoteEndpoint = (IPEndPoint)socket.RemoteEndPoint;

            WriteLock = new SemaphoreSlim(1, 1);
            isShutdown = 0;

            this.logger = logger;
        }

        /// <summary>
        /// Creates an EpoxyNetworkStream from a TCP socket, handling failures that may occur during
        /// TCP/TLS connection establishment.
        /// </summary>
        /// <remarks>
        /// This is a helper function that centralizes error handling during connection creation.
        /// </remarks>
        /// <param name="socketFunc">A function to invoke to acquire a socket.</param>
        /// <param name="streamFunc">
        /// A function to invoke to wrap a socket in a network stream.
        /// </param>
        /// <param name="timeoutConfig">The timeout config to use for this stream.</param>
        /// <param name="logger">The logger.</param>
        /// <returns>A connected EpoxyNetworkStream.</returns>
        public static async Task<EpoxyNetworkStream> MakeAsync(
            Func<Task<Socket>> socketFunc,
            Func<Socket, Task<EpoxyNetworkStream>> streamFunc,
            EpoxyTransport.TimeoutConfig timeoutConfig,
            Logger logger)
        {
            Socket socket = null;

            try
            {
                socket = await socketFunc();
                ConfigureSocketKeepAlive(socket, timeoutConfig, logger);

                return await streamFunc(socket);
            }
            catch (Exception)
            {
                SafeShutdownSocket(socket, logger);
                throw;
            }
        }

        /// <summary>
        /// Creates a connected client-side EpoxyNetworkStream from a connected TCP socket.
        /// </summary>
        /// <remarks>
        /// If TLS is enabled, this will establish the TLS connection. Upon successful return, the
        /// stream owns the socket.
        /// </remarks>
        /// <param name="remoteHostname">
        /// The expected name of the remote host. Used during certificate validation.
        /// </param>
        /// <param name="socket">The socket to wrap.</param>
        /// <param name="tlsConfig">
        /// TLS config. May be <c>null</c> to indicate a plain-text connection.
        /// </param>
        /// <param name="logger">A logger.</param>
        /// <returns>An connected EpoxyNetworkStream.</returns>
        public static async Task<EpoxyNetworkStream> MakeClientStreamAsync(
            string remoteHostname,
            Socket socket,
            EpoxyClientTlsConfig tlsConfig,
            Logger logger)
        {
            Stream clientStream;
            NetworkStream networkStream = new NetworkStream(socket, ownsSocket: false);

            if (tlsConfig == null)
            {
                clientStream = networkStream;
            }
            else
            {
                const bool leaveInnerStreamOpen = false;

                var sslStream = new SslStream(
                    networkStream,
                    leaveInnerStreamOpen,
                    tlsConfig.RemoteCertificateValidationCallback);

                var clientCertificates = new X509CertificateCollection();
                if (tlsConfig.Certificate != null)
                {
                    clientCertificates.Add(tlsConfig.Certificate);
                }

                await sslStream.AuthenticateAsClientAsync(
                    remoteHostname,
                    clientCertificates,
                    AllowedTlsProtocols,
                    tlsConfig.CheckCertificateRevocation);

                logger.Site().Debug("Authenticated connection to {0}[{1}]", remoteHostname, socket.RemoteEndPoint);

                clientStream = sslStream;
            }

            return new EpoxyNetworkStream(socket, clientStream, logger);
        }

        /// <summary>
        /// Creates a connected server-side EpoxyNetworkStream from a connected TCP socket.
        /// </summary>
        /// <remarks>
        /// If TLS is enabled, this will establish the TLS connection. Upon successful return, the
        /// stream owns the socket.
        /// </remarks>
        /// <param name="socket">The socket to wrap.</param>
        /// <param name="tlsConfig">
        /// TLS config. May be <c>null</c> to indicate a plain-text connection.
        /// </param>
        /// <param name="logger">A logger.</param>
        /// <returns>An connected EpoxyNetworkStream.</returns>
        public static async Task<EpoxyNetworkStream> MakeServerStreamAsync(
            Socket socket,
            EpoxyServerTlsConfig tlsConfig,
            Logger logger)
        {
            Stream serverStream;
            var networkStream = new NetworkStream(socket, ownsSocket: false);

            if (tlsConfig == null)
            {
                serverStream = networkStream;
            }
            else
            {
                const bool leaveInnerStreamOpen = false;

                var sslStream = new SslStream(
                    networkStream,
                    leaveInnerStreamOpen,
                    MakeServerCertificateValidationCallback(tlsConfig, logger));

                await sslStream.AuthenticateAsServerAsync(
                    tlsConfig.Certificate,
                    tlsConfig.ClientCertificateRequired,
                    enabledSslProtocols: AllowedTlsProtocols,
                    checkCertificateRevocation: tlsConfig.CheckCertificateRevocation);

                if (tlsConfig.ClientCertificateRequired && !sslStream.IsMutuallyAuthenticated)
                {
                    sslStream.Dispose();
                    throw new AuthenticationException("Mutual authentication was required, but it could not be performed.");
                }

                logger.Site().Debug(
                    "Authenticated connection from {0}. Mutually authenticated?: {1}",
                    socket.RemoteEndPoint,
                    sslStream.IsMutuallyAuthenticated);

                serverStream = sslStream;
            }

            return new EpoxyNetworkStream(socket, serverStream, logger);
        }

        /// <summary>
        /// Gets a <see cref="Stream"/> that can be used for network I/O.
        /// </summary>
        public Stream Stream
        {
            get
            {
                if (isShutdown != 0)
                {
                    throw new ObjectDisposedException(nameof(EpoxyNetworkStream));
                }

                return stream;
            }
        }

        /// <summary>
        /// Shuts down the connection.
        /// </summary>
        /// <remarks>
        /// Shutdown is idempotent and may be called multiple times.
        /// </remarks>
        public void Shutdown()
        {
            int oldIsShutdown = Interlocked.CompareExchange(ref isShutdown, 1, 0);
            if (oldIsShutdown == 0)
            {
                // we are responsible for shutdown
                try
                {
                    stream.Dispose();

                    try
                    {
                        socket.Shutdown(SocketShutdown.Send);
                        socket.Close();
                    }
                    catch (ObjectDisposedException)
                    {
                        // ignore, as we're shutting down anyway
                    }

                    // We cannot call socket.Disconnect, as that will block
                    // for longer than we want. So, we just forcible close
                    // the socket with Dispose
                    socket.Dispose();
                }
                catch (Exception ex) when (ex is IOException || ex is SocketException)
                {
                    logger.Site().Error(ex, "Exception during connection shutdown");
                }

                stream = null;
                socket = null;
                logger = null;
            }
        }

        static RemoteCertificateValidationCallback MakeServerCertificateValidationCallback(
            EpoxyServerTlsConfig tlsConfig,
            Logger logger)
        {
            if (tlsConfig.ClientCertificateRequired)
            {
                // If client certificates are required, then add an explicit
                // check that the client provided a certificate. The default
                // behavior is to allow the connection even if the client
                // didn't present a certificate.
                return (sender, certificate, chain, errors) =>
                {
                    if (certificate == null)
                    {
                        logger.Site().Error("Rejecting client. Certificate required, but client did not provide one.");
                        return false;
                    }

                    if (tlsConfig.RemoteCertificateValidationCallback != null)
                    {
                        // There's a user-provided validation callback, so
                        // delegate to that.
                        return tlsConfig.RemoteCertificateValidationCallback(
                            sender,
                            certificate,
                            chain,
                            errors);
                    }
                    else
                    {
                        // Otherwise, require no errors at all to accept the
                        // certificate.
                        return errors == SslPolicyErrors.None;
                    }
                };
            }
            else
            {
                // Client certificates are not required, so just use the
                // user-provided validation callback. This may be null, but
                // that's fine. SslStream will just use its default behavior
                // then.
                return tlsConfig.RemoteCertificateValidationCallback;
            }
        }

        static void ConfigureSocketKeepAlive(
            Socket socket,
            EpoxyTransport.TimeoutConfig timeoutConfig,
            Logger logger)
        {
            if (timeoutConfig.KeepAliveTime != TimeSpan.Zero && timeoutConfig.KeepAliveInterval != TimeSpan.Zero)
            {
                // Socket.IOControl for IOControlCode.KeepAliveValues is expecting a structure like
                // the following on Windows:
                //
                // struct tcp_keepalive
                // {
                //     u_long onoff; // 0 for off, non-zero for on
                //     u_long keepalivetime; // milliseconds
                //     u_long keepaliveinterval; // milliseconds
                // };
                //
                // On some platforms this gets mapped to the relevant OS structures, but on other
                // platforms, this may fail with a PlatformNotSupportedException.
                UInt32 keepAliveTimeMillis = checked((UInt32)timeoutConfig.KeepAliveTime.TotalMilliseconds);
                UInt32 keepAliveIntervalMillis = checked((UInt32)timeoutConfig.KeepAliveInterval.TotalMilliseconds);

                var keepAliveVals = new byte[sizeof(UInt32) * 3];
                keepAliveVals[0] = 1;

                keepAliveVals[4] = (byte)(keepAliveTimeMillis & 0xff);
                keepAliveVals[5] = (byte)((keepAliveTimeMillis >> 8) & 0xff);
                keepAliveVals[6] = (byte)((keepAliveTimeMillis >> 16) & 0xff);
                keepAliveVals[7] = (byte)((keepAliveTimeMillis >> 24) & 0xff);

                keepAliveVals[8] = (byte)(keepAliveIntervalMillis & 0xff);
                keepAliveVals[9] = (byte)((keepAliveIntervalMillis >> 8) & 0xff);
                keepAliveVals[10] = (byte)((keepAliveIntervalMillis >> 16) & 0xff);
                keepAliveVals[11] = (byte)((keepAliveIntervalMillis >> 24) & 0xff);

                try
                {
                    socket.IOControl(IOControlCode.KeepAliveValues, keepAliveVals, null);
                }
                catch (ObjectDisposedException)
                {
                    // Oh well: the connection went down before we could configure it. Nothing to be
                    // done, except to wait for the next socket operation to fail and let normal
                    // clean up take over.
                }
                catch (Exception ex) when (ex is SocketException || ex is PlatformNotSupportedException)
                {
                    logger.Site().Warning(ex, "Socket keep-alive could not be configured");
                }
            }
        }

        static void SafeShutdownSocket(Socket socket, Logger logger)
        {
            try
            {
                socket?.Shutdown(SocketShutdown.Both);
                socket?.Close();
            }
            catch (SocketException ex)
            {
                // We tried to cleanly shutdown the socket, oh well.
                logger.Site().Debug(ex, "Exception encountered when shutting down a socket.");
            }
        }
    }
}
