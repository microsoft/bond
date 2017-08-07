package com.microsoft.bond.examples;

import Examples.Color;
import Examples.Fruit;
import Examples.Limits;

public class Enumerations {

    public static final int UINT32_MIN = 0;

    public static void main(final String[] args) {

        Color yellow = Color.Yellow;
        assert yellow.getValue() == 2;

        Fruit apple = Fruit.Apple;
        assert apple.getValue() == 1;

        assert Limits.Int32Min.getValue() == Integer.MIN_VALUE;
        assert Limits.Int32Max.getValue() == Integer.MAX_VALUE;
        assert Limits.UInt32Min.getValue() == 0; 
        // TODO: handle Limits.UInt32Max
    }

}
