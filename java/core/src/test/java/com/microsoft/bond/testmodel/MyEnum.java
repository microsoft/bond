// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond.testmodel;

import com.microsoft.bond.*;

/**
 * Bond enum type used for testing, hand-crafted to match generated code.
 */
public final class MyEnum implements BondEnum<MyEnum> {

    // defines constants
    public static final class Values {

        private Values() {
        }

        public static final int White = 1;
        public static final int Yellow = 2;
        public static final int Orange = 3;
        public static final int Purple = 4;
        public static final int Blue = 5;
        public static final int Green = 6;
        public static final int Brown = 7;
        public static final int Black = 8;
    }

    // type descriptor implementation
    private static final class EnumBondTypeImpl extends EnumBondType<MyEnum> {

        @Override
        public java.lang.Class<MyEnum> getValueClass() {
            return MyEnum.class;
        }

        @Override
        public final MyEnum getEnumValue(int value) {
            return get(value);
        }
    }

    public static final MyEnum White = new MyEnum(Values.White, "White");
    public static final MyEnum Yellow = new MyEnum(Values.Yellow, "Yellow");
    public static final MyEnum Orange = new MyEnum(Values.Orange, "Orange");
    public static final MyEnum Purple = new MyEnum(Values.Purple, "Purple");
    public static final MyEnum Blue = new MyEnum(Values.Blue, "Blue");
    public static final MyEnum Green = new MyEnum(Values.Green, "Green");
    public static final MyEnum Brown = new MyEnum(Values.Brown, "Brown");
    public static final MyEnum Black = new MyEnum(Values.Black, "Black");

    ///////////////////////////////////////////////////////////////////////////
    // Bond enum static members
    ///////////////////////////////////////////////////////////////////////////

    public static final EnumBondType<MyEnum> BOND_TYPE = new EnumBondTypeImpl();

    public static MyEnum get(int value) {
        switch (value) {
            case Values.White:
                return White;
            case Values.Yellow:
                return Yellow;
            case Values.Orange:
                return Orange;
            case Values.Purple:
                return Purple;
            case Values.Blue:
                return Blue;
            case Values.Green:
                return Green;
            case Values.Brown:
                return Brown;
            case Values.Black:
                return Black;
            default:
                return new MyEnum(value, null);
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Bond enum instance members
    ///////////////////////////////////////////////////////////////////////////

    public final int value;
    private final String label;

    private MyEnum(int value, String label) {
        this.value = value;
        this.label = label;
    }

    @Override
    public final int getValue() {
        return this.value;
    }

    @Override
    public final EnumBondType<MyEnum> getBondType() {
        return BOND_TYPE;
    }

    @Override
    public final int hashCode() {
        return super.hashCode();
    }

    @Override
    public final boolean equals(Object obj) {
        return (obj instanceof MyEnum) && (this.value == ((MyEnum) obj).value);
    }

    @Override
    public String toString() {
        return (this.label != null) ? this.label : ("MyEnum(" + String.valueOf(this.value) + ")");
    }

    @Override
    public final int compareTo(MyEnum o) {
        return (this.value < o.value) ? -1 : ((this.value > o.value) ? 1 : 0);
    }
}
