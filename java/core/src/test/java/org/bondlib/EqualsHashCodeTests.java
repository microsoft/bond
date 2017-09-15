package org.bondlib;

import org.bondlib.Blob;
import org.bondlib.test.*;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;

public class EqualsHashCodeTests {
    // a.equals(b) == true -> a.hashCode() == b.hashCode()
    private <T> void assertObjectsAreEqual(T one, T another) {
        Assert.assertEquals(one, another);
        Assert.assertEquals(one.hashCode(), another.hashCode());
    }

    @Test
    public void reflexivity() {
        final Empty e = new Empty();
        final Primitives p = new Primitives();
        final Containers c = new Containers();
        final Recursive r = new Recursive();
        final Derived d = new Derived();
        final Aliased a = new Aliased();

        assertObjectsAreEqual(e, e);
        assertObjectsAreEqual(p, p);
        assertObjectsAreEqual(c, c);
        assertObjectsAreEqual(r, r);
        assertObjectsAreEqual(d, d);
        assertObjectsAreEqual(a, a);
    }

    @Test
    public void defaultObjects() {
        assertObjectsAreEqual(new Empty(), new Empty());
        assertObjectsAreEqual(new Primitives(), new Primitives());
        assertObjectsAreEqual(new Containers(), new Containers());
        assertObjectsAreEqual(new Recursive(), new Recursive());
        assertObjectsAreEqual(new Derived(), new Derived());
        assertObjectsAreEqual(new Aliased(), new Aliased());
    }

    @Test
    public void primitiveFields() {
        final Primitives one = new Primitives(), another = new Primitives();
        for (final Primitives p : Arrays.asList(one, another)) {
            p.iint8 = 3;
            p.iint16 = 4;
            p.iint32 = 5;
            p.iint64 = 6;
            p.iuint8 = 7;
            p.iuint16 = 8;
            p.iuint32 = 9;
            p.iuint64 = 10;
            p.ffloat = 11.0f;
            p.fdouble = 12.0;
            p.bbool = true;
            p.str = "thirteen";
            p.wstr = "fourteen";
        }
        assertObjectsAreEqual(one, another);

        // Break each field's equality.
        another.iint8 = (byte) (one.iint8 + 1);
        Assert.assertNotEquals(one, another);
        another.iint8 = one.iint8;

        another.iint16 = (short) (one.iint16 + 1);
        Assert.assertNotEquals(one, another);
        another.iint16 = one.iint16;

        another.iint32 = one.iint32 + 1;
        Assert.assertNotEquals(one, another);
        another.iint32 = one.iint32;

        another.iint64 = one.iint64 + 1;
        Assert.assertNotEquals(one, another);
        another.iint64 = one.iint64;

        another.iuint8 = (byte) (one.iuint8 + 1);
        Assert.assertNotEquals(one, another);
        another.iuint8 = one.iuint8;

        another.iuint16 = (short) (one.iuint16 + 1);
        Assert.assertNotEquals(one, another);
        another.iuint16 = one.iuint16;

        another.iuint32 = one.iuint32 + 1;
        Assert.assertNotEquals(one, another);
        another.iuint32 = one.iuint32;

        another.iuint64 = one.iuint64 + 1;
        Assert.assertNotEquals(one, another);
        another.iuint64 = one.iuint64;

        another.ffloat = one.ffloat + 1.0f;
        Assert.assertNotEquals(one, another);
        another.ffloat = one.ffloat;

        another.fdouble = one.fdouble + 1.0f;
        Assert.assertNotEquals(one, another);
        another.fdouble = one.fdouble;

        another.bbool = !one.bbool;
        Assert.assertNotEquals(one, another);
        another.bbool = one.bbool;

        another.str = one.str + "eee";
        Assert.assertNotEquals(one, another);
        another.str = one.str;

        another.wstr = one.wstr + "eee";
        Assert.assertNotEquals(one, another);
        another.wstr = one.wstr;
    }

    @Test
    public void containerFields() {
        final Containers one = new Containers(), another = new Containers();
        for (final Containers c : Arrays.asList(one, another)) {
            c.intList.add(1);
            c.intList.add(2);

            c.intVector.add(2);
            c.intVector.add(3);

            c.intSet.add(4);
            c.intSet.add(5);

            c.intMap.put(6, 7);
            c.intMap.put(8, 9);

            final Primitives p1 = new Primitives(), p2 = new Primitives();
            p1.iint64 = 10;
            p2.iint64 = 11;
            c.primitivesList.add(p1);
            c.primitivesList.add(p2);

            c.dataBlob = new Blob(new byte[]{12, 13});
        }
        assertObjectsAreEqual(one, another);

        // Break each container's equality.
        another.intList.clear();
        Assert.assertNotEquals(one, another);
        another.intList.addAll(one.intList);

        another.intVector.clear();
        Assert.assertNotEquals(one, another);
        another.intVector.addAll(one.intVector);

        another.intSet.clear();
        Assert.assertNotEquals(one, another);
        another.intSet.addAll(one.intSet);

        another.intMap.clear();
        Assert.assertNotEquals(one, another);
        another.intMap.putAll(one.intMap);

        another.primitivesList.get(0).iint64++;
        Assert.assertNotEquals(one, another);
        another.primitivesList.get(0).iint64 = one.primitivesList.get(0).iint64;

        another.dataBlob.getData()[0]++;
        Assert.assertNotEquals(one, another);
        another.dataBlob.getData()[0] = one.dataBlob.getData()[0];
    }

    @Test
    public void recursiveFields() {
        final Recursive one = new Recursive(), another = new Recursive();
        for (final Recursive r : Arrays.asList(one, another)) {
            r.depth = 0;
            r.r = new Recursive();
            r.r.depth = 1;
        }
        assertObjectsAreEqual(one, another);

        // Break recursive field equality.
        another.r.depth = one.r.depth + 1;
        Assert.assertNotEquals(one, another);
        another.r.depth = one.r.depth;
    }

    @Test
    public void inheritance() {
        final Derived one = new Derived(), another = new Derived();
        for (final Derived d : Arrays.asList(one, another)) {
            d.derivedInt = 1;
            d.baseInt = 2;
        }
        assertObjectsAreEqual(one, another);

        // Break derived class field equality.
        another.derivedInt = one.derivedInt + 1;
        Assert.assertNotEquals(one, another);
        another.derivedInt = one.derivedInt;

        // Break base class field equality.
        another.baseInt = one.baseInt + 1;
        Assert.assertNotEquals(one, another);
        another.baseInt = one.baseInt;
    }

    @Test
    public void alias() {
        final Aliased one = new Aliased(), another = new Aliased();
        for (final Aliased a : Arrays.asList(one, another)) {
            a.d = 3;
        }
        assertObjectsAreEqual(one, another);

        // Break equality in the aliased field.
        another.d = one.d + 1;
        Assert.assertNotEquals(one, another);
        another.d = one.d;
    }
}
