// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Protocols
{
    using System;
    using Newtonsoft.Json;

    public interface IJsonReader
    {
        void Read();

        void Skip();

        bool EOF { get; }
        
        JsonToken TokenType { get; }

        object Value { get; }

        int LineNumber { get; }

        int LinePosition { get; }
    }
}
