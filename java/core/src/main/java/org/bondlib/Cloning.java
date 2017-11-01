package org.bondlib;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;

final class Cloning {
    // A ByteArrayInputStream can be duplicated with the three pieces of
    // state that are passed to its long-form constructor. Unfortunately,
    // they are hidden in protected members after construction, so we need
    // to reflect to get them out.
    //
    // There's one more piece of state inside that class - mark. We don't
    // use mark()/reset(), and cloned streams shouldn't leak out of the
    // library, so we ignore it here.

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

    // A FileInputStream can be "cloned" by mmap'ing the underlying file and
    // creating a stream around the MappedByteBuffer that results. The JDK does
    // not include a stream for ByteBuffers, so we have a simple one of our
    // own: ByteBufferInputStream. In the event that we have a bonded in a
    // bonded, ByteBufferInputStream is actually cloneable.

    static class ByteBufferInputStream extends InputStream {
        // Wrap a ByteBuffer in a stream, but track position and size outside
        // of it so that we can clone this stream, too.
        final ByteBuffer buffer;
        final long size;
        long position;

        ByteBufferInputStream(final ByteBuffer buffer, final long position, final long size) {
            this.buffer = buffer;
            this.position = position;
            this.size = size;
        }

        @Override
        public int read() throws IOException {
            if (this.position == this.size) {
                return -1;
            }

            final byte raw = this.buffer.get((int) this.position++);
            return UnsignedHelper.asUnsignedInt(raw);
        }
    }

    static ByteBufferInputStream clone(FileInputStream stream) throws IOException {
        final FileChannel channel = stream.getChannel();
        final long size = channel.size();
        final long position = channel.position();
        final MappedByteBuffer mapped = channel.map(FileChannel.MapMode.READ_ONLY, 0, size);
        return new ByteBufferInputStream(mapped, position, size);
    }

    static ByteBufferInputStream clone(ByteBufferInputStream stream) {
        return new ByteBufferInputStream(stream.buffer, stream.position, stream.size);
    }

    static InputStream cloneStream(InputStream stream) throws IOException {
        if (stream instanceof FileInputStream) {
            return clone((FileInputStream) stream);
        } else if (stream instanceof ByteBufferInputStream) {
            return clone((ByteBufferInputStream) stream);
        } else if (stream instanceof ByteArrayInputStream) {
            return clone((ByteArrayInputStream) stream);
        } else {
            throw new StreamNotCloneableException("Stream is not cloneable: " + stream);
        }
    }
}
