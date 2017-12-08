// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Grpc
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Reflection;
    using NUnit.Framework;
    using NUnit.Framework.Constraints;

    [TestFixture]
    class AttributesTests
    {
        private static Dictionary<string, Dictionary<string, string>> ExpectedMethodAttributes = new Dictionary
            <string, Dictionary<string, string>>
            {
                {
                    "IntToInt",
                    new Dictionary<string, string>
                    {
                        {"SomeAttribute", "method value"},
                        {
                            "DifferentAttribute", string.Empty
                        }
                    }
                },
                {
                    "NothingToInt",
                    new Dictionary<string, string>
                    {
                        {"NothingToInt", "no clash"}
                    }
                },
                {"IntToNothing", new Dictionary<string, string>()},
                {"NothingToNothing", new Dictionary<string, string>()}
            };

        [Test]
        public void ServiceAttributes_CanBeFound()
        {
            var serviceTypeInfo = typeof(SimpleService).GetTypeInfo();
            var bondAttributes =
                serviceTypeInfo.GetCustomAttributes<Bond.AttributeAttribute>()
                    .ToDictionary(ba => ba.Name, ba => ba.Value);

            CollectionAssert.AreEquivalent(
                new Dictionary<string, string> {{"SomeAttribute", "service value"}},
                bondAttributes);
        }

        [Test]
        public void MethodAttributes_OnServiceBase_CanBeFound()
        {
            var serviceBaseTypeInfo = typeof(SimpleService.SimpleServiceBase).GetTypeInfo();
            var methodAttributes = serviceBaseTypeInfo.DeclaredMethods.Where(mi => mi.IsPublic && mi.IsAbstract)
                .ToDictionary(mi => mi.Name, CollectMethodAttributes);

            CollectionAssert.AreEquivalent(ExpectedMethodAttributes, methodAttributes);
        }

        [Test]
        public void MethodAttributes_OnClient_CanBeFound()
        {
            var clientTypeInfo = typeof(SimpleService.SimpleServiceClient).GetTypeInfo();
            var clientMethods = clientTypeInfo.DeclaredMethods.Where(mi => mi.IsPublic && mi.IsVirtual);

            foreach (MethodInfo mi in clientMethods)
            {
                string grpcMethodName = ClientNameToGrpcMethodName(mi.Name);

                Dictionary<string, string> expected;
                bool found = ExpectedMethodAttributes.TryGetValue(grpcMethodName, out expected);
                Assert.IsTrue(found, "No expected attributes found for '{0}'", grpcMethodName);

                Dictionary<string, string> actual = CollectMethodAttributes(mi);

                CollectionAssert.AreEquivalent(expected, actual, "Attribute mismatch for '{0}'", grpcMethodName);
            }
        }

        static Dictionary<string, string> CollectMethodAttributes(MethodInfo mi)
        {
            return mi.GetCustomAttributes<Bond.AttributeAttribute>().ToDictionary(ba => ba.Name, ba => ba.Value);
        }

        static string ClientNameToGrpcMethodName(string clientName)
        {
            const string clientNameSuffix = "Async";
            Assert.That(clientName, new EndsWithConstraint(clientNameSuffix));
            return clientName.Substring(0, clientName.Length - clientNameSuffix.Length);
        }
    }
}
