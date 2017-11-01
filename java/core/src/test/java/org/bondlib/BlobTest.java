package org.bondlib;

import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;

public class BlobTest {

    @Test
    public void equalsAndHashCode()
    {
        final byte[] data = new byte[256];
        for (int i = 0; i < data.length; i++) {
            data[i] = (byte) i;
        }

        final Blob blob1 = new Blob(data);
        final Blob blob2 = new Blob(data);
        final Blob blob3 = new Blob(Arrays.copyOf(data, data.length));
        final Blob blob4 = new Blob(Arrays.copyOfRange(data, 10, 15));

        Assert.assertEquals(blob1, blob2);
        Assert.assertEquals(blob1.hashCode(), blob2.hashCode());
        Assert.assertEquals(blob1, blob3);
        Assert.assertEquals(blob1.hashCode(), blob3.hashCode());

        Assert.assertNotEquals(blob1, blob4);
    }
}
