// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.IO
{
    /// <summary>
    /// Clones an input stream or protocol reader
    /// </summary>
    /// <typeparam name="T">Object type</typeparam>
    public interface ICloneable<T>
    {
        /// <summary>
        /// Create a clone
        /// </summary>
        T Clone();
    }
}
