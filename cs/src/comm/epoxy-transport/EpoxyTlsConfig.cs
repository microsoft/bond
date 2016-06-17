// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Net.Security;
    using System.Security.Authentication;
    using System.Security.Cryptography.X509Certificates;

    /// <summary>
    /// Represents join client- and server-side TLS options.
    /// </summary>
    public abstract class EpoxyTlsConfig
    {
        /// <summary>
        /// Creates a new instance of the <see cref="EpoxyTlsConfig"/> class.
        /// </summary>
        /// <param name="remoteCertificateValidationCallback">
        /// Optional delegate responsible for validating remote certificates.
        /// May be <c>null</c> to use the default validation.
        /// </param>
        /// <param name="encryptionPolicy">
        /// The encryption policy to use. Defaults to
        /// <see cref="System.Net.Security.EncryptionPolicy.RequireEncryption">EncryptionPolicy.RequireEncryption</see>
        /// </param>
        /// <param name="enabledProtocols">
        /// The enabled protocols. Defaults to
        /// <see cref="SslProtocols.Tls12"/>.
        /// </param>
        /// <param name="checkCertificateRevocation">
        /// Whether certificate revocation is checked. Defaults to <c>true</c>.
        /// </param>
        protected EpoxyTlsConfig(
            RemoteCertificateValidationCallback remoteCertificateValidationCallback = null,
            EncryptionPolicy encryptionPolicy = EncryptionPolicy.RequireEncryption,
            SslProtocols enabledProtocols = SslProtocols.Tls12,
            bool checkCertificateRevocation = true)
        {
            RemoteCertificateValidationCallback = remoteCertificateValidationCallback;
            EncryptionPolicy = encryptionPolicy;
            EnabledProtocols = enabledProtocols;
            CheckCertificateRevocation = checkCertificateRevocation;
        }

        /// <summary>
        /// Gets an optional delegate responsible for validating remote
        /// certificates.
        /// </summary>
        public RemoteCertificateValidationCallback RemoteCertificateValidationCallback { get; }

        /// <summary>
        /// Gets the encryption policy to use.
        /// </summary>
        public EncryptionPolicy EncryptionPolicy { get; }

        /// <summary>
        /// Gets the enabled protocols.
        /// </summary>
        public SslProtocols EnabledProtocols { get; }

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
        /// Optional delegate responsible for validating remote certificates.
        /// May be <c>null</c> to use the default validation.
        /// </param>
        /// <param name="encryptionPolicy">
        /// The encryption policy to use. Defaults to
        /// <see cref="System.Net.Security.EncryptionPolicy.RequireEncryption">EncryptionPolicy.RequireEncryption</see>
        /// </param>
        /// <param name="enabledProtocols">
        /// The enabled protocols. Defaults to
        /// <see cref="SslProtocols.Tls12"/>.
        /// </param>
        /// <param name="checkCertificateRevocation">
        /// Whether certificate revocation is checked. Defaults to <c>true</c>.
        /// </param>
        public EpoxyServerTlsConfig(
            X509Certificate certificate,
            bool clientCertificateRequired = false,
            RemoteCertificateValidationCallback remoteCertificateValidationCallback = null,
            EncryptionPolicy encryptionPolicy = EncryptionPolicy.RequireEncryption,
            SslProtocols enabledProtocols = SslProtocols.Tls12,
            bool checkCertificateRevocation = true)
            : base(
                remoteCertificateValidationCallback: remoteCertificateValidationCallback,
                encryptionPolicy: encryptionPolicy,
                enabledProtocols: enabledProtocols,
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
        /// Optional delegate responsible for validating remote certificates.
        /// May be <c>null</c> to use the default validation.
        /// </param>
        /// <param name="encryptionPolicy">
        /// The encryption policy to use. Defaults to
        /// <see cref="System.Net.Security.EncryptionPolicy.RequireEncryption">EncryptionPolicy.RequireEncryption</see>
        /// </param>
        /// <param name="enabledProtocols">
        /// The enabled protocols. Defaults to
        /// <see cref="SslProtocols.Tls12"/>.
        /// </param>
        /// <param name="checkCertificateRevocation">
        /// Whether certificate revocation is checked. Defaults to <c>true</c>.
        /// </param>
        public EpoxyClientTlsConfig(X509Certificate certificate = null,
            RemoteCertificateValidationCallback remoteCertificateValidationCallback = null,
            EncryptionPolicy encryptionPolicy = EncryptionPolicy.RequireEncryption,
            SslProtocols enabledProtocols = SslProtocols.Tls12,
            bool checkCertificateRevocation = true)
            : base(
                remoteCertificateValidationCallback: remoteCertificateValidationCallback,
                encryptionPolicy: encryptionPolicy,
                enabledProtocols: enabledProtocols,
                checkCertificateRevocation: checkCertificateRevocation)
        {
            Certificate = certificate;
        }

        /// <summary>
        /// Gets the certificate used to authenticate when acting as a client.
        /// </summary>
        public X509Certificate Certificate { get; }
    }
}
