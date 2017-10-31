package org.bondlib;

import org.junit.Assert;
import org.junit.Test;

import java.io.*;
import java.lang.reflect.Field;
import java.net.URISyntaxException;
import java.net.URL;

import static org.junit.Assert.fail;

public class CloningTest {
    //
    // reflection assumptions
    //
    private static class PrivateInt {
        private int i;
    }

    @Test
    public void fieldAccessibilityIsolation() {
        // We depend on the assumption that we can modify accessibility through
        // our java.lang.reflect.Field instances without interfering with or
        // interference from other components.

        final Field accessible;
        final Field inaccessible;
        try {
            accessible = PrivateInt.class.getDeclaredField("i");
            inaccessible = PrivateInt.class.getDeclaredField("i");
        } catch (final NoSuchFieldException e) {
            fail("Couldn't get field - did the test class definition change?");
            return;     // javac doesn't realize fail() doesn't return
        }

        // Make only one field accessible...
        accessible.setAccessible(true);
        // ... then try to access the private field with the other.
        boolean getThrew = false;
        try {
            inaccessible.getInt(new PrivateInt());
        } catch (final IllegalAccessException e) {
            getThrew = true;
        }

        Assert.assertTrue("accessing private field through unmodified Field should have thrown", getThrew);
    }

    //
    // common code for stream cloning
    //
    private interface Cloner<S extends InputStream> {
        InputStream clone(final S stream);
    }

    private interface ExpectedGetter {
        byte get(final int i);
    }

    private <S extends InputStream> void compareStreams(
            final S original, final Cloner<S> cloner,
            final ExpectedGetter expected,
            final int readFirst, final int length) throws IOException {
        // Skip zero or more bytes before cloning.
        int i = 0;
        for (; i < readFirst; i++) {
            try {
                StreamHelper.readByte(original);
            } catch (final IOException ignored) {
                fail("original stream hit end-of-stream while skipping initial bytes");
            }
        }

        final InputStream cloned = cloner.clone(original);
        // original and cloned should see the same sequence of bytes from here.
        for (; i < length; i++) {
            final int originalInt = original.read();
            if (originalInt == -1) {
                fail(String.format("original stream hit end-of-stream at index %d", i));
            }
            final byte originalByte = (byte) originalInt;

            final int clonedInt = cloned.read();
            if (clonedInt == -1) {
                fail(String.format("cloned stream hit end-of-stream at index %d", i));
            }
            final byte clonedByte = (byte) clonedInt;

            final byte expectedByte = expected.get(i);
            Assert.assertEquals(String.format("original stream read the wrong value at index %d", i),
                expectedByte, originalByte);
            Assert.assertEquals(String.format("cloned stream read the wrong value at index %d", i),
                expectedByte, clonedByte);
        }

        // original and cloned should both signal end-of-stream on their next reads.
        Assert.assertTrue("original stream read past the end of its segment", original.read() == -1);
        Assert.assertTrue("cloned stream read past the end of its segment", cloned.read() == -1);
    }

    //
    // ByteArrayInputStream cloning
    //
    private static final byte[] bytes = new byte[] {
        0x01, 0x02, 0x03,
        0x04, 0x05, 0x06,
        0x07, 0x08, 0x09,
        0x0a, 0x0b, 0x0c
    };

    private static class ByteArrayInputStreamCloner implements Cloner<ByteArrayInputStream> {
        @Override
        public InputStream clone(ByteArrayInputStream stream) {
            return Cloning.clone(stream);
        }
    }

    private static class ByteArrayInputStreamExpectedGetter implements ExpectedGetter {
        private final int offset;

        ByteArrayInputStreamExpectedGetter(final int offset) {
            this.offset = offset;
        }

        @Override
        public byte get(int i) {
            return bytes[this.offset + i];
        }
    }

    @Test
    public void byteArrayInputStream_fieldAccessibility() throws NoSuchFieldException, IllegalAccessException {
        // We need to be able to access these fields by reflection to clone a
        // ByteArrayInputStream. They have been part of ByteArrayInputStream
        // since JDK 1.0, so something interesting has happened if this test
        // throws.

        final Field bufField = ByteArrayInputStream.class.getDeclaredField("buf");
        final Field posField = ByteArrayInputStream.class.getDeclaredField("pos");
        final Field countField = ByteArrayInputStream.class.getDeclaredField("count");

        bufField.setAccessible(true);
        posField.setAccessible(true);
        countField.setAccessible(true);

        final ByteArrayInputStream bais = new ByteArrayInputStream(new byte[]{});
        bufField.get(bais);
        posField.getInt(bais);
        countField.getInt(bais);
    }

    @Test
    public void byteArrayInput() throws IOException {
        final ByteArrayInputStreamCloner cloner = new ByteArrayInputStreamCloner();
        final int segmentLength = bytes.length / 2;

        // stream consumes entire array
        final ByteArrayInputStreamExpectedGetter wholeArrayGetter = new ByteArrayInputStreamExpectedGetter(0);
        compareStreams(
            new ByteArrayInputStream(bytes, 0, bytes.length), cloner, wholeArrayGetter,
            0, bytes.length);
        compareStreams(
            new ByteArrayInputStream(bytes, 0, bytes.length), cloner, wholeArrayGetter,
            bytes.length / 2, bytes.length);
        compareStreams(
            new ByteArrayInputStream(bytes, 0, bytes.length), cloner, wholeArrayGetter,
            bytes.length, bytes.length);

        // stream consumes prefix of array
        final ByteArrayInputStreamExpectedGetter prefixGetter = wholeArrayGetter;
        compareStreams(
            new ByteArrayInputStream(bytes, 0, segmentLength), cloner, prefixGetter,
            0, segmentLength);
        compareStreams(
            new ByteArrayInputStream(bytes, 0, segmentLength), cloner, prefixGetter,
            segmentLength / 2, segmentLength);
        compareStreams(
            new ByteArrayInputStream(bytes, 0, segmentLength), cloner, prefixGetter,
            segmentLength, segmentLength);

        // stream consumes suffix of array
        final ByteArrayInputStreamExpectedGetter suffixGetter =
            new ByteArrayInputStreamExpectedGetter(bytes.length - segmentLength);
        compareStreams(
            new ByteArrayInputStream(bytes, bytes.length - segmentLength, segmentLength), cloner, suffixGetter,
            0, segmentLength);
        compareStreams(
            new ByteArrayInputStream(bytes, bytes.length - segmentLength, segmentLength), cloner, suffixGetter,
            segmentLength / 2, segmentLength);
        compareStreams(
            new ByteArrayInputStream(bytes, bytes.length - segmentLength, segmentLength), cloner, suffixGetter,
            segmentLength, segmentLength);

        // stream consumes middle of array
        final int middleSegmentOffset = segmentLength / 2;
        final ByteArrayInputStreamExpectedGetter middleGetter =
            new ByteArrayInputStreamExpectedGetter(middleSegmentOffset);
        compareStreams(
            new ByteArrayInputStream(bytes, middleSegmentOffset, segmentLength), cloner, middleGetter,
            0, segmentLength);
        compareStreams(
            new ByteArrayInputStream(bytes, middleSegmentOffset, segmentLength), cloner, middleGetter,
            segmentLength / 2, segmentLength);
        compareStreams(
            new ByteArrayInputStream(bytes, middleSegmentOffset, segmentLength), cloner, middleGetter,
            segmentLength, segmentLength);
    }

    //
    // FileInputStream cloning
    //
    private static final String testTxtPath = "/alphabet.txt";
    private static final byte[] testTxtContents = StringHelper.encodeString("ABCDEFGHIJKLMNOPQRSTUVWXYZ");

    private static class FileInputStreamCloner implements Cloner<FileInputStream> {
        public InputStream clone(final FileInputStream stream) {
            try {
                return Cloning.clone(stream);
            } catch (final IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

    private static class ByteBufferInputStreamCloner implements Cloner<Cloning.ByteBufferInputStream> {

        @Override
        public InputStream clone(Cloning.ByteBufferInputStream stream) {
            return Cloning.clone(stream);
        }
    }

    private static class FileInputStreamExpectedGetter implements ExpectedGetter {
        @Override
        public byte get(int i) {
            return testTxtContents[i];
        }
    }

    private FileInputStream testFileStream() {
        try {
            final URL url = getClass().getResource(testTxtPath);
            final File file = new File(url.toURI());
            return new FileInputStream(file);
        } catch (final IOException e) {
            throw new RuntimeException(e);
        } catch (final URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    public void testTxtPreconditions() throws IOException, URISyntaxException {
        // We need a file to test FileInputStream cloning. This test asserts
        // the properties of the test file that the others will assume.

        // Check resource availability.
        final FileInputStream stream = testFileStream();

        // Check file length.
        final byte[] contents = new byte[testTxtContents.length];
        Assert.assertEquals(contents.length, stream.read(contents));
        // This should be the end of the file.
        Assert.assertEquals(-1, stream.read());

        // Check file content.
        Assert.assertArrayEquals(testTxtContents, contents);
    }

    @Test
    public void fileInput() throws IOException {
        final FileInputStreamCloner cloner = new FileInputStreamCloner();
        final FileInputStreamExpectedGetter getter = new FileInputStreamExpectedGetter();

        compareStreams(testFileStream(), cloner, getter, 0, testTxtContents.length);
        compareStreams(testFileStream(), cloner, getter, testTxtContents.length / 2, testTxtContents.length);
        compareStreams(testFileStream(), cloner, getter, testTxtContents.length, testTxtContents.length);
    }

    @Test
    public void byteBufferInput() throws IOException {
        final ByteBufferInputStreamCloner cloner = new ByteBufferInputStreamCloner();
        final FileInputStreamExpectedGetter getter = new FileInputStreamExpectedGetter();

        compareStreams(Cloning.clone(testFileStream()), cloner, getter,
            0, testTxtContents.length);
        compareStreams(Cloning.clone(testFileStream()), cloner, getter,
            testTxtContents.length / 2, testTxtContents.length);
        compareStreams(Cloning.clone(testFileStream()), cloner, getter,
            testTxtContents.length, testTxtContents.length);
    }
}
