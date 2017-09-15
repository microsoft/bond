// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * A metadata describing a struct, used by {@link ProtocolWriter} for certain protocol types.
 */
public interface StructMetadata {

    /**
     * Retrieves the struct name not qualified with namespace.
     *
     * @return struct name
     */
    String getName();

    /**
     * Retrieves the struct name qualified with namespace.
     *
     * @return struct qualified name
     */
    String getQualifiedName();
}
