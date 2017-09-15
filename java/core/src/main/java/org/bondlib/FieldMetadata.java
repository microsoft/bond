// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * A metadata describing a struct field, used by {@link ProtocolWriter} for certain protocol types.
 */
public interface FieldMetadata {

    /**
     * Gets the field ID.
     *
     * @return the field ID
     */
    short getId();

    /**
     * Gets the field name.
     *
     * @return the field name
     */
    String getName();

    /**
     * Gets the field modifier.
     *
     * @return the field modifier
     */
    Modifier getModifier();

    /**
     * Gets a value indicating whether the field is defaulting to "nothing", which means that its
     * value is wrapped by the {@link Something} wrapper.
     *
     * @return a value indicating whether the field is defaulting to "nothing"
     */
    boolean isDefaultNothing();

    /**
     * Gets the default value of the field if the field is of primitive data type, or null otherwise.
     *
     * @return the default value of the field or null
     */
    Object getDefaultValue();
}
