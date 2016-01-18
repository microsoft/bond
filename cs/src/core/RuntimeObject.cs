// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System.Collections.Generic;

    /// <summary>
    /// Represents an object created using a <see cref="RuntimeSchema"/> object without a corresponding .NET type.
    /// </summary>
    [Schema]
    public class RuntimeObject
    {
        private readonly Dictionary<string, object> _properties = new Dictionary<string, object>();

        /// <summary>
        /// Gets a dictionary containing properties of this object.
        /// </summary>
        public Dictionary<string, object> Properties
        {
            get { return _properties; }
        }
    }
}
