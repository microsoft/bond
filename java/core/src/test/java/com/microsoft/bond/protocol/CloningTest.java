package com.microsoft.bond.protocol;

import org.junit.Assert;
import org.junit.Test;
import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;

public class CloningTest {
    private final byte[] bytes = new byte[] {
            0x01, 0x02, 0x03,
            0x04, 0x05, 0x06,
            0x07, 0x08, 0x09,
            0x0a, 0x0b, 0x0c
    };

    private void byteArrayInput(final int readFirst, final int offset, final int length) {
        final ByteArrayInputStream original = new ByteArrayInputStream(bytes, offset, length);

        // Skip zero or more bytes before cloning.
        int i = 0;
        for (; i < readFirst; i++) {
            try {
                StreamHelper.readByte(original);
            } catch (final IOException ignored) {
                fail("original stream hit end-of-stream while skipping initial bytes");
            }
        }

        // original and cloned should see the same sequence of bytes from here.
        final ByteArrayInputStream cloned = Cloning.clone(original);
        for (; i < length; i++) {
            // readByte() converts the underlying representation of end-of-stream
            // to an IOException. A byte[]-backed stream shouldn't throw for any
            // other reason.
            byte originalByte = 0;
            try {
                originalByte = StreamHelper.readByte(original);
            } catch (final IOException ignored) {
                fail(String.format("original stream hit end-of-stream at index %d", i));
            }

            byte clonedByte = 0;
            try {
                clonedByte = StreamHelper.readByte(cloned);
            } catch (final IOException ignored) {
                fail(String.format("cloned stream hit end-of-stream at index %d", i));
            }

            final byte expectedByte = bytes[offset + i];
            Assert.assertEquals(String.format("original stream read the wrong value at index %d", i),
                    expectedByte, originalByte);
            Assert.assertEquals(String.format("cloned stream read the wrong value at index %d", i),
                    expectedByte, clonedByte);
        }

        // original and cloned should both signal end-of-stream on their next reads.
        boolean originalThrewEof = false;
        try {
            StreamHelper.readByte(original);
        } catch (final IOException ignored) {
            originalThrewEof = true;
        }
        Assert.assertTrue("original stream read past the end of its segment", originalThrewEof);

        boolean clonedThrewEof = false;
        try {
            StreamHelper.readByte(cloned);
        } catch (final IOException ignored) {
            clonedThrewEof = true;
        }
        Assert.assertTrue("cloned stream read past the end of its segment", clonedThrewEof);
    }

    @Test
    public void byteArrayInput() {
        // stream consumes entire array
        byteArrayInput(0, 0, bytes.length);
        byteArrayInput(bytes.length / 2, 0, bytes.length);
        byteArrayInput(bytes.length, 0, bytes.length);

        final int segmentLength = bytes.length / 2;

        // stream consumes prefix of array
        byteArrayInput(0, 0, segmentLength);
        byteArrayInput(segmentLength / 2, 0, segmentLength);
        byteArrayInput(segmentLength, 0, segmentLength);

        // stream consumes suffix of array
        byteArrayInput(0, bytes.length - segmentLength, segmentLength);
        byteArrayInput(segmentLength / 2, bytes.length - segmentLength, segmentLength);
        byteArrayInput(segmentLength, bytes.length - segmentLength, segmentLength);

        // stream consumes middle of array
        final int middleSegmentOffset = segmentLength / 2;
        byteArrayInput(0, middleSegmentOffset, segmentLength);
        byteArrayInput(segmentLength / 2, middleSegmentOffset, segmentLength);
        byteArrayInput(segmentLength, middleSegmentOffset, segmentLength);
    }
}
