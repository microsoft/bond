package org.bondlib.examples;

// See build.gradle for namespace mapping
import org.bondlib.examples.enumerations.Color;
import org.bondlib.examples.enumerations.Fruit;
import org.bondlib.examples.enumerations.Limits;

public class Enumerations {

    public static void main(final String[] args) {

        Color yellow = Color.Yellow;
        assert yellow.getValue() == 2;

        Fruit apple = Fruit.Apple;
        assert apple.getValue() == 1;

        assert Limits.Int32Min.getValue() == Integer.MIN_VALUE;
        assert Limits.Int32Max.getValue() == Integer.MAX_VALUE;
        assert Limits.UInt32Min.getValue() == 0;

        // Enum values greater than Integer.MAX_VALUE will cast to their signed equivalents.
        assert Limits.UInt32Max.getValue() == -1;
    }

}
