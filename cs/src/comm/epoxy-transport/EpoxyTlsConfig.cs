// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Net.Security;
    using System.Security.Cryptography.X509Certificates;

    /// <summary>
    /// Represents TLS options common to client and server configurations.
    /// </summary>
    public abstract class EpoxyTlsConfig
    {
        /// <summary>
        /// Creates a new instance of the <see cref="EpoxyTlsConfig"/> class.
        /// </summary>
        /// <param name="remoteCertificateValidationCallback">
        /// Normal use is to omit this parameter/pass <c>null</c> so that the
        /// system's default behavior is used. Passing something other than
        /// null here is almost always a bug. This delegate is passed to
        /// <see cref="SslStream"/>; consult the SslStream documentation for
        /// more details about its use.
        /// </param>
        /// <param name="checkCertificateRevocation">
        /// Whether certificate revocation is checked. Defaults to <c>true</c>.
        /// </param>
        protected EpoxyTlsConfig(
            RemoteCertificateValidationCallback remoteCertificateValidationCallback = null,
            bool checkCertificateRevocation = true)
        {
            RemoteCertificateValidationCallback = remoteCertificateValidationCallback;
            CheckCertificateRevocation = checkCertificateRevocation;
        }

        /// <summary>
        /// Gets an optional delegate responsible for validating remote
        /// certificates.
        /// </summary>
        public RemoteCertificateValidationCallback RemoteCertificateValidationCallback { get; }

        /// <summary>
        /// Gets a flag indicating whether certificate revocation is checked.
        /// </summary>
        public bool CheckCertificateRevocation { get; }
    }

    /// <summary>
    /// Represents server-side TLS options.
    /// </summary>
    public class EpoxyServerTlsConfig : EpoxyTlsConfig
    {
        /// <summary>
        /// Creates a new instance of the <see cref="EpoxyServerTlsConfig"/>
        /// class, which is used to configure TLS paramaters for
        /// <see cref="EpoxyListener"/> instances.
        /// </summary>
        /// <param name="certificate">
        /// The certificate used to identify this server. May not be <c>null</c>.
        /// </param>
        /// <param name="clientCertificateRequired">
        /// Whether clients are required to present a certificate. Defaults to
        /// <c>false</c>.
        /// </param>
        /// <param name="remoteCertificateValidationCallback">
        /// Normal use is to omit this parameter/pass <c>null</c> so that the
        /// system's default behavior is used. Passing something other than
        /// null here is almost always a bug. This delegate is passed to
        /// <see cref="SslStream"/>; consult the SslStream documentation for
        /// more details about its use.
        /// </param>
        /// <param name="checkCertificateRevocation">
        /// Whether certificate revocation is checked. Defaults to <c>true</c>.
        /// </param>
        public EpoxyServerTlsConfig(
            X509Certificate certificate,
            bool clientCertificateRequired = false,
            RemoteCertificateValidationCallback remoteCertificateValidationCallback = null,
            bool checkCertificateRevocation = true)
            : base(
                remoteCertificateValidationCallback: remoteCertificateValidationCallback,
                checkCertificateRevocation: checkCertificateRevocation)
        {
            if (certificate == null)
            {
                throw new ArgumentNullException(nameof(certificate));
            }

            Certificate = certificate;
            ClientCertificateRequired = clientCertificateRequired;
        }

        /// <summary>
        /// Gets the certificate used to authenticate when acting as a server.
        /// </summary>
        public X509Certificate Certificate { get; }

        /// <summary>
        /// Gets a flag indicating whether the server will require a
        /// certificate from any clients that connect.
        /// </summary>
        public bool ClientCertificateRequired { get; }
    }

    /// <summary>
    /// Represents client-side TLS options.
    /// </summary>
    public class EpoxyClientTlsConfig : EpoxyTlsConfig
    {
        /// <summary>
        /// The default client configuration.
        /// </summary>
        public static readonly EpoxyClientTlsConfig Default = new EpoxyClientTlsConfig();

        /// <summary>
        /// Creates a new instance of the <see cref="EpoxyClientTlsConfig"/>
        /// class, which is used to configure TLS paramaters for client
        /// connections.
        /// </summary>
        /// <param name="certificate">
        /// The certificate used to identify this client to servers. Defaults
        /// to <c>null</c>.
        /// </param>
        /// <param name="remoteCertificateValidationCallback">
        /// Normal use is to omit this parameter/pass <c>null</c> so that the
        /// system's default behavior is used. Passing something other than
        /// null here is almost always a bug. This delegate is passed to
        /// <see cref="SslStream"/>; consult the SslStream documentation for
        /// more details about its use.
        /// </param>
        /// <param name="checkCertificateRevocation">
        /// Whether certificate revocation is checked. Defaults to <c>true</c>.
        /// </param>
        public EpoxyClientTlsConfig(
            X509Certificate2 certificate = null,
            RemoteCertificateValidationCallback remoteCertificateValidationCallback = null,
            bool checkCertificateRevocation = true)
            : base(
                remoteCertificateValidationCallback: remoteCertificateValidationCallback,
                checkCertificateRevocation: checkCertificateRevocation)
        {
            Certificate = certificate;
        }

        /// <summary>
        /// Gets the certificate used to authenticate when acting as a client.
        /// </summary>
        public X509Certificate2 Certificate { get; }
    }
}
