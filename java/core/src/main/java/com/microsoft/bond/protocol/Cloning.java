package com.microsoft.bond.protocol;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Field;

class Cloning {
    private static final Field byteArrayInputStream_buf;
    private static final Field byteArrayInputStream_pos;
    private static final Field byteArrayInputStream_count;

    static {
        try {
            byteArrayInputStream_buf = ByteArrayInputStream.class.getDeclaredField("buf");
            byteArrayInputStream_pos = ByteArrayInputStream.class.getDeclaredField("pos");
            byteArrayInputStream_count = ByteArrayInputStream.class.getDeclaredField("count");
        } catch (final NoSuchFieldException e) {
            // This should never happen. ByteArrayInputStream has had these
            // fields since JDK 1.0.
            throw new RuntimeException(e);
        }

        // These changes are scoped to our Field instances and need not be cleaned up.
        byteArrayInputStream_buf.setAccessible(true);
        byteArrayInputStream_pos.setAccessible(true);
        byteArrayInputStream_count.setAccessible(true);
    }

    static ByteArrayInputStream clone(ByteArrayInputStream stream) {
        // A ByteArrayInputStream can be duplicated with the three pieces of
        // state that are passed to its long-form constructor. Unfortunately,
        // they are hidden in protected members after construction, so we need
        // to reflect to get them out.
        //
        // There's one more piece of state inside that class - mark. We don't
        // use mark()/reset(), and cloned streams shouldn't leak out of the
        // library, so we ignore it here.
        final byte[] buf;
        final int pos;
        final int count;

        try {
            buf = (byte[]) byteArrayInputStream_buf.get(stream);
            pos = byteArrayInputStream_pos.getInt(stream);
            count = byteArrayInputStream_count.getInt(stream);
        } catch (final IllegalAccessException e) {
            // We made these accessible in this class's static block.
            throw new RuntimeException(e);
        }

        return new ByteArrayInputStream(buf, pos, count - pos);
    }
}
