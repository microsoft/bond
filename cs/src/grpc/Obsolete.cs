// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Grpc
{
    using System;

    [Obsolete(message: Value, error: false)]
    internal static class ObsoleteMessage
    {
        internal const string Value = "Bond-over-gRPC will be removed in the next major version of Bond. See https://github.com/microsoft/bond/issues/1131";
    }
}