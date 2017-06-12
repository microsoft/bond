package com.microsoft.bond.protocol;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Field;

class Cloning {
    private static final Object byteArrayInputStreamLock = new Object();
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

        synchronized (byteArrayInputStreamLock) {
            byteArrayInputStream_buf.setAccessible(true);
            byteArrayInputStream_pos.setAccessible(true);
            byteArrayInputStream_count.setAccessible(true);

            try {
                buf = (byte[]) byteArrayInputStream_buf.get(stream);
                pos = byteArrayInputStream_pos.getInt(stream);
                count = byteArrayInputStream_count.getInt(stream);
            } catch (final IllegalAccessException e) {
                // This synchronized block makes this expose/access/hide
                // operation safe to call concurrently, but there's no way to
                // guarantee unrelated code isn't modifying these fields'
                // accessibility. In that unlikely event, there's nothing
                // sensible we can do to recover.
                throw new RuntimeException(e);
            }

            // The RuntimeException caused by an IllegalAccessException thrown
            // above indicates a serious incompatibility between this part of
            // Bond and some other component and cannot be resolved
            // programmatically.
            //
            // We can't even clean up without a risk of causing the same
            // problem to the other component, so we do this outside the try,
            // not in a finally {}.
            byteArrayInputStream_buf.setAccessible(false);
            byteArrayInputStream_pos.setAccessible(false);
            byteArrayInputStream_count.setAccessible(false);
        }

        return new ByteArrayInputStream(buf, pos, count - pos);
    }
}
