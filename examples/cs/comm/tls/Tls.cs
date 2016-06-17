// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Examples.Tls
{
    using System;
    using System.Linq;
    using System.Net;
    using System.Security.Cryptography.X509Certificates;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;

    public static class Tls
    {
        static readonly IPEndPoint serviceEndpoint = new IPEndPoint(IPAddress.Loopback, EpoxyTransport.DefaultSecurePort);

        public static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Usage();
                Environment.Exit(1);
            }

            string serverCertficiateFriendlyName = args[0];
            if (serverCertficiateFriendlyName.Equals("help", StringComparison.InvariantCultureIgnoreCase)
                || serverCertficiateFriendlyName.Equals("-?", StringComparison.InvariantCultureIgnoreCase)
                || serverCertficiateFriendlyName.Equals("/?", StringComparison.InvariantCultureIgnoreCase)
                || serverCertficiateFriendlyName.Equals("-help", StringComparison.InvariantCultureIgnoreCase)
                || serverCertficiateFriendlyName.Equals("--help", StringComparison.InvariantCultureIgnoreCase))
            {
                Usage();
                Environment.Exit(1);
            }

            X509Certificate2 serverCertificate = FindCertificateInStore(serverCertficiateFriendlyName, StoreLocation.CurrentUser);
            serverCertificate = serverCertificate ?? FindCertificateInStore(serverCertficiateFriendlyName, StoreLocation.LocalMachine);

            if (serverCertificate == null)
            {
                Console.Error.WriteLine("Could not find any certificates with friendly name [{0}] in either the user store or the machine store", serverCertficiateFriendlyName);
                Environment.Exit(2);
            }

            Task.Run(() => MainAsync(serverCertificate)).Wait();
        }

        static void Usage()
        {
            Console.WriteLine("tls server-certificate-friendly-name");
            Console.WriteLine("   Looks for a certificate with the friendly name server-certificate-friendly-name\n" +
                              "   first in the user certificte store and then in the system store.");
        }

        static X509Certificate2 FindCertificateInStore(string friendlyname, StoreLocation whichStore)
        {
            var store = new X509Store(whichStore);
            store.Open(OpenFlags.OpenExistingOnly | OpenFlags.ReadOnly);

            return store.Certificates
                .Cast<X509Certificate2>()
                .FirstOrDefault(certificate => certificate.FriendlyName.Equals(friendlyname, StringComparison.InvariantCultureIgnoreCase));
        }

        static async Task MainAsync(X509Certificate2 serverCertificate)
        {
            var tlsConfig = new EpoxyServerTlsConfig(serverCertificate);

            EpoxyTransport transport = new EpoxyTransportBuilder().SetServerTlsConfig(tlsConfig).Construct();

            EpoxyListener listener = transport.MakeListener(serviceEndpoint);
            listener.AddService(new SimpleService());
            await listener.StartAsync();

            var connection = await transport.ConnectToAsync("epoxys://localhost");
            var proxy = new SimpleProxy<EpoxyConnection>(connection);
            IMessage<SimpleResult> response = await proxy.SimpleMethodAsync();

            PrintResponse(response);
        }

        static void PrintResponse(IMessage<SimpleResult> response)
        {
            if (response.IsError)
            {
                Error err = response.Error.Deserialize();
                Console.WriteLine("Error {0}: {1}", err.error_code, err.message);
            }
            else
            {
                SimpleResult result = response.Payload.Deserialize();
                Console.WriteLine("Result: {0}", result.int_value);
            }
        }
    }
}
