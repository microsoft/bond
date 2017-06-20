// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond.helpers;

/**
 * Contains helper methods for argument error checking.
 */
public final class ArgumentHelper {

    // prevent instantiation
    private ArgumentHelper() {
    }

    /**
     * Checks if the argument is null and throws {@link IllegalArgumentException} if it is.
     *
     * @param argument the argument to check
     * @param name     the name of the argument
     * @throws IllegalArgumentException if the argument is null
     */
    public static void ensureNotNull(Object argument, String name) {
        if (argument == null) {
            throw new IllegalArgumentException("Argument '" + name + "' must not be null.");
        }
    }
}