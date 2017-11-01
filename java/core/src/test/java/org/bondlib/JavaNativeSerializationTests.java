package org.bondlib;

import org.bondlib.test.*;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.*;

public class JavaNativeSerializationTests {
    private ByteArrayOutputStream baos;
    private ObjectOutputStream oos;

    @Before
    public void setup() throws IOException {
        this.baos = new ByteArrayOutputStream();
        this.oos = new ObjectOutputStream(baos);
    }

    @Test
    public void generatedTypesAreExternalizable() {
        //noinspection ConstantConditions
        Assert.assertTrue(new Base() instanceof Externalizable);
    }

    @Test
    public void simple() throws IOException, ClassNotFoundException {
        final Base baseIn = new Base();
        baseIn.baseInt = 1;
        oos.writeObject(baseIn);

        final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
        final Base baseOut = (Base) ois.readObject();
        Assert.assertEquals(baseIn, baseOut);
    }

    @Test
    public void inheritance() throws IOException, ClassNotFoundException {
        final Derived derivedIn = new Derived();
        derivedIn.baseInt = 1;
        derivedIn.derivedInt = 2;
        oos.writeObject(derivedIn);

        final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
        final Derived derivedOut = (Derived) ois.readObject();
        Assert.assertEquals(derivedIn, derivedOut);
    }

    @Test
    public void bonded() throws IOException, ClassNotFoundException, IllegalAccessException {
        final Base baseIn = new Base();
        baseIn.baseInt = 1;
        final HasBondedField hbfIn = new HasBondedField();
        hbfIn.bondedField = Bonded.fromObject(baseIn, Base.BOND_TYPE);
        oos.writeObject(hbfIn);

        final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
        final HasBondedField hbfOut = (HasBondedField) ois.readObject();
        TestHelper.assertStructMemberwiseEquals(hbfIn, hbfOut);
    }

    @Test
    public void concreteGenericField() throws IOException, ClassNotFoundException, IllegalAccessException {
        final ConcreteWithGenericField concreteIn = new ConcreteWithGenericField();
        concreteIn.concreteContainerField.valueField.baseInt = 1;
        oos.writeObject(concreteIn);

        final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
        final ConcreteWithGenericField concreteOut = (ConcreteWithGenericField) ois.readObject();
        TestHelper.assertStructMemberwiseEquals(concreteIn, concreteOut);
    }

    @Test
    public void genericThrows() throws IOException, ClassNotFoundException, IllegalAccessException {
        final StructBondType<GenericValueContainer<Integer>> containerBondType =
            GenericValueContainer.BOND_TYPE.makeGenericType(Int32BondType.INSTANCE);
        final GenericValueContainer<Integer> containerIn = new GenericValueContainer<Integer>(containerBondType);
        containerIn.valueField = 1;
        try {
            oos.writeObject(containerIn);
            Assert.fail("serializing a generic should have thrown");
        } catch (IllegalArgumentException ignored) {}

        final ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
        try {
            ois.readObject();
            Assert.fail("deserializing a generic should have thrown");
        } catch (InvalidClassException ignored) {}
    }
}
