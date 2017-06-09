package com.microsoft.bond.protocol;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Field;

class Cloning {
    static ByteArrayInputStream clone(ByteArrayInputStream stream) {
        // A ByteArrayInputStream can be duplicated with the three pieces of
        // state that are passed to its long-form constructor. Unfortunately,
        // they are hidden in protected members after construction, so we need
        // to reflect to get them out.
        //
        // There's one more piece of state inside that class - mark. We don't
        // use mark()/reset(), and cloned streams shouldn't leak out of the
        // library, so we ignore it here.

        final Field bufField;
        final Field posField;
        final Field countField;
        try {
            bufField = ByteArrayInputStream.class.getDeclaredField("buf");
            posField = ByteArrayInputStream.class.getDeclaredField("pos");
            countField = ByteArrayInputStream.class.getDeclaredField("count");
        } catch (final NoSuchFieldException e) {
            // This should never happen. ByteArrayInputStream has been in the
            // JDK and has had these fields since 1.0.
            throw new RuntimeException(e);
        }

        bufField.setAccessible(true);
        posField.setAccessible(true);
        countField.setAccessible(true);

        final byte[] buf;
        final int pos;
        final int count;
        try {
            buf = (byte[]) bufField.get(stream);
            pos = posField.getInt(stream);
            count = countField.getInt(stream);
        } catch (final IllegalAccessException e) {
            // This should never happen. We just made those fields accessible.
            throw new RuntimeException(e);
        }

        bufField.setAccessible(false);
        posField.setAccessible(false);
        countField.setAccessible(false);

        final ByteArrayInputStream cloned = new ByteArrayInputStream(buf, 0, count);
        cloned.skip(pos);
        return cloned;
    }
}
