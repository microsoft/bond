// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Protocols
{
    public static class XmlMetadata
    {
        public static string GetXmlNamespace(this Metadata metadata)
        {
            string xmlns;
            if (metadata.attributes.TryGetValue("xmlns", out xmlns))
                return xmlns;

            return "urn:" + metadata.qualified_name.EncodeXmlName();
        }

        public static string GetXmlName(this Metadata metadata)
        {
            return metadata.name.EncodeXmlName();
        }

        public static string EncodeXmlName(this string name)
        {
            return name.Replace('<', '_').Replace('>', '_').Replace(',', '_').Replace(' ', '_');
        }
    }
}
