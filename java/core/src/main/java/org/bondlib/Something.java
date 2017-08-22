// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * A generic wrapper around the value of a field defaulting to "nothing" in the Bond schema.
 * @param <T> the type of the wrapped value
 */
public abstract class Something<T> {

    // restrict subclasses to this package only
    Something() {
    }

    /**
     * Gets the value.
     *
     * @return the value.
     */
    public abstract T getValue();

    /**
     * Sets the value.
     *
     * @param value the value
     */
    public abstract void setValue(T value);

    @Override
    public final String toString() {
        return String.valueOf(this.getValue());
    }

    /**
     * Wraps the given value by (@see {@link Something} wrapper.
     *
     * @param value the value to wrap
     * @param <T>   the type of the value to wrap
     * @return a new instance of the wrapper
     */
    public static <T> SomethingObject<T> wrap(T value) {
        return new SomethingObject<T>(value);
    }

    /**
     * Wraps the given value by (@see {@link Something} wrapper.
     *
     * @param value the value to wrap
     * @return a new instance of the wrapper
     */
    public static SomethingByte wrap(byte value) {
        return new SomethingByte(value);
    }

    /**
     * Wraps the given value by (@see {@link Something} wrapper.
     *
     * @param value the value to wrap
     * @return a new instance of the wrapper
     */
    public static SomethingShort wrap(short value) {
        return new SomethingShort(value);
    }

    /**
     * Wraps the given value by (@see {@link Something} wrapper.
     *
     * @param value the value to wrap
     * @return a new instance of the wrapper
     */
    public static SomethingInteger wrap(int value) {
        return new SomethingInteger(value);
    }

    /**
     * Wraps the given value by (@see {@link Something} wrapper.
     *
     * @param value the value to wrap
     * @return a new instance of the wrapper
     */
    public static SomethingLong wrap(long value) {
        return new SomethingLong(value);
    }

    /**
     * Wraps the given value by (@see {@link Something} wrapper.
     *
     * @param value the value to wrap
     * @return a new instance of the wrapper
     */
    public static SomethingBoolean wrap(boolean value) {
        return new SomethingBoolean(value);
    }

    /**
     * Wraps the given value by (@see {@link Something} wrapper.
     *
     * @param value the value to wrap
     * @return a new instance of the wrapper
     */
    public static SomethingFloat wrap(float value) {
        return new SomethingFloat(value);
    }

    /**
     * Wraps the given value by (@see {@link Something} wrapper.
     *
     * @param value the value to wrap
     * @return a new instance of the wrapper
     */
    public static SomethingDouble wrap(double value) {
        return new SomethingDouble(value);
    }

    /**
     * If the argument value is not null returns the wrapped value or returns null otherwise.
     *
     * @param wrappedValue the wrapped value
     * @param <T>          the type of the wrapped value
     * @return the wrapped value or null
     */
    public static <T> T unwrap(Something<? extends T> wrappedValue) {
        return unwrap(wrappedValue, null);
    }

    /**
     * If the argument value is not null returns the wrapped value or returns the fallback value otherwise.
     *
     * @param wrappedValue  the wrapped value
     * @param fallbackValue the fallback value
     * @param <T>           the type of the wrapped value
     * @return the wrapped value or the fallback value
     */
    public static <T> T unwrap(Something<? extends T> wrappedValue, T fallbackValue) {
        return wrappedValue != null ? wrappedValue.getValue() : fallbackValue;
    }

    /**
     * If the argument value is not null returns the wrapped value or returns the fallback value otherwise.
     *
     * @param wrappedValue  the wrapped value
     * @param fallbackValue the fallback value
     * @return the wrapped value or the fallback value
     */
    public static byte unwrap(SomethingByte wrappedValue, byte fallbackValue) {
        return wrappedValue != null ? wrappedValue.value : fallbackValue;
    }

    /**
     * If the argument value is not null returns the wrapped value or returns the fallback value otherwise.
     *
     * @param wrappedValue  the wrapped value
     * @param fallbackValue the fallback value
     * @return the wrapped value or the fallback value
     */
    public static short unwrap(SomethingShort wrappedValue, short fallbackValue) {
        return wrappedValue != null ? wrappedValue.value : fallbackValue;
    }

    /**
     * If the argument value is not null returns the wrapped value or returns the fallback value otherwise.
     *
     * @param wrappedValue  the wrapped value
     * @param fallbackValue the fallback value
     * @return the wrapped value or the fallback value
     */
    public static int unwrap(SomethingInteger wrappedValue, int fallbackValue) {
        return wrappedValue != null ? wrappedValue.value : fallbackValue;
    }

    /**
     * If the argument value is not null returns the wrapped value or returns the fallback value otherwise.
     *
     * @param wrappedValue  the wrapped value
     * @param fallbackValue the fallback value
     * @return the wrapped value or the fallback value
     */
    public static long unwrap(SomethingLong wrappedValue, long fallbackValue) {
        return wrappedValue != null ? wrappedValue.value : fallbackValue;
    }

    /**
     * If the argument value is not null returns the wrapped value or returns the fallback value otherwise.
     *
     * @param wrappedValue  the wrapped value
     * @param fallbackValue the fallback value
     * @return the wrapped value or the fallback value
     */
    public static boolean unwrap(SomethingBoolean wrappedValue, boolean fallbackValue) {
        return wrappedValue != null ? wrappedValue.value : fallbackValue;
    }

    /**
     * If the argument value is not null returns the wrapped value or returns the fallback value otherwise.
     *
     * @param wrappedValue  the wrapped value
     * @param fallbackValue the fallback value
     * @return the wrapped value or the fallback value
     */
    public static float unwrap(SomethingFloat wrappedValue, float fallbackValue) {
        return wrappedValue != null ? wrappedValue.value : fallbackValue;
    }

    /**
     * If the argument value is not null returns the wrapped value or returns the fallback value otherwise.
     *
     * @param wrappedValue  the wrapped value
     * @param fallbackValue the fallback value
     * @return the wrapped value or the fallback value
     */
    public static double unwrap(SomethingDouble wrappedValue, double fallbackValue) {
        return wrappedValue != null ? wrappedValue.value : fallbackValue;
    }
}
