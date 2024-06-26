// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * Provides controls that impact how deserialization behaves.
 */
public final class DeserializerControls {

    private static int maxDepth = 64;

    /**
     * Gets the maximum recursion depth allowed.
     *
     * @return the max depth
     */
    public static int getMaxDepth() {
        return maxDepth;
    }

    /**
     * Sets the maximum recursion depth allowed.
     *
     * @param value the new max depth
     * @throws IllegalArgumentException if the argument was zero or negative
     */
    public static void setMaxDepth(int value) {
        if (value <= 0) {
            throw new IllegalArgumentException("Argument value must be positive");
        }

        maxDepth = value;
    }

    private final static ThreadLocal<Integer> currentDepth = new ThreadLocal<Integer>() {
        @Override
        protected Integer initialValue() {
            return 0;
        }
    };

    final static int validateDepthForIncrement() throws InvalidBondDataException {
        int depth = currentDepth.get();
        if (depth >= maxDepth) {
            throw new InvalidBondDataException("Recursion depth exceeded max depth");
        }

        return depth;
    }

    final static void setDepth(int depth) {
        currentDepth.set(depth);
    }
}
