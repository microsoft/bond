import unittest
import random
import string
import functools
import sys
from python_unit_test import Serialize, Deserialize, Marshal, Unmarshal, GetRuntimeSchema
import python_unit_test as test

def atleast_python3():
    return sys.version_info[0] >= 3

def random_string():
    return ''.join(random.sample(string.ascii_lowercase*16, 16))

def random_blob():
    if atleast_python3():
        return bytes(random_string(), 'ascii')
    else:
        return random_string()

def random_uint(bits):
    return random.randint(0, (1 << bits) - 1)

def random_int(bits):
    return random_uint(bits) - (1 << (bits - 1))

random_bool = functools.partial(random.choice, [True, False])
random_int8 = functools.partial(random_int, 8)
random_int16 = functools.partial(random_int, 16)
random_int32 = functools.partial(random_int, 32)
random_int64 = functools.partial(random_int, 64)
random_uint8 = functools.partial(random_uint, 8)
random_uint16 = functools.partial(random_uint, 16)
random_uint32 = functools.partial(random_uint, 32)
random_uint64 = functools.partial(random_uint, 64)

def random_list(random_element):
    l = []
    for i in range(0, random.randint(1, 7)):
        l.append(random_element())
    return l

def random_set(random_element):
    return set(random_list(random_element))

def random_map(random_key, random_value):
    return dict(zip(random_list(random_key), random_list(random_value)))

def serialize_deserialize(obj):
    data = Serialize(obj)

    obj_type = type(obj)
    new_obj = obj_type()

    Deserialize(data, new_obj)
    return new_obj

def marshal_unmarshal(obj):
    data = Marshal(obj)

    obj_type = type(obj)
    new_obj = obj_type()

    Unmarshal(data, new_obj)

    data2 = Marshal(new_obj)
    new_obj2 = obj_type()

    Unmarshal(data2, new_obj2, GetRuntimeSchema(obj))
    return new_obj2

class BondTest(unittest.TestCase):

    def initSimpleStruct(self, obj):
        assert isinstance(obj, test.SimpleStruct)
        obj.m_bool = random_bool()
        obj.m_str = random_string()
        obj.m_wstr = random_string()
        obj.m_int8 = random_int(8)
        obj.m_int16 = random_int(16)
        obj.m_int32 = random_int(32)
        obj.m_int64 = random_int(64)
        obj.m_uint8 = random_uint(8)
        obj.m_uint16 = random_uint(16)
        obj.m_uint32 = random_uint(32)
        obj.m_uint64 = random_uint(64)
        obj.m_double = random.random()
        obj.m_float = random.random()
        obj.m_enum1 = random.choice([ \
            test.EnumType1.EnumValue1, \
            test.EnumType1.EnumValue3, \
            test.EnumType1.EnumValue4, \
            test.EnumType1.EnumValue5])
        obj.m_blob = random_blob()

    def randomSimpleStruct(self):
        obj = test.SimpleStruct()
        self.initSimpleStruct(obj)
        return obj

    def initSimpleWithBase(self, obj):
        # BUGBUG: For fields that are hidden by overrides from derived class
        # initSimpleStruct will set values of derived, not of SimpleStruct.
        # Below we set those fields for the base. Ideally it should be the
        # other way around, initSimpleStruct should set fields of base and here
        # we would set the derived overrides. Need to figure out how to achieve
        # this in Python...
        self.initSimpleStruct(obj)
        test.SimpleStruct.m_int32.__set__(obj, random_int(32))
        test.SimpleStruct.m_enum1.__set__(obj, random.choice([ \
            test.EnumType1.EnumValue1, \
            test.EnumType1.EnumValue3, \
            test.EnumType1.EnumValue4, \
            test.EnumType1.EnumValue5]))

    def randomSimpleWithBase(self):
        obj = test.SimpleWithBase()
        self.initSimpleWithBase(obj)
        return obj

    def initSimpleContainers(self, obj):
        obj.l_bool = random_list(random_bool)
        obj.l_uint32 = random_list(random_uint32)
        obj.l_string = random_list(random_string)
        obj.v_bool = random_list(random_bool)
        obj.v_uint8 = random_list(random_uint8)
        obj.v_double = random_list(random.random)
        obj.v_string = random_list(random_string)
        obj.s_uint64 = random_set(random_uint64)
        obj.s_string = random_set(random_string)
        obj.m_int8_string = random_map(random_int8, random_string)
        obj.m_float_uint16 = random_map(random.random, random_uint16)

    def randomSimpleContainers(self):
        obj = test.SimpleContainers()
        self.initSimpleContainers(obj)
        return obj

    def initNullable(self, obj):
        obj.nullable_list = random_list(random.random)
        obj.nullable_struct = test.SimpleStruct()
        obj.nullable_map = random_map(random_int8, random_int8)
        obj.nullable_string = random_string()
        obj.nullable_blob = random_blob()
        obj.nullable_nullable_uint32 = random_uint(32)
        self.assertNotEqual(None, obj.nullable_list)
        self.assertNotEqual(None, obj.nullable_struct)
        self.assertNotEqual(None, obj.nullable_map)
        self.assertNotEqual(None, obj.nullable_string)
        self.assertNotEqual(None, obj.nullable_blob)
        self.assertNotEqual(None, obj.nullable_nullable_uint32)
        obj.list_nullable_struct = [None, self.randomSimpleStruct()]
        self.assertEqual(None, obj.list_nullable_struct[0])
        self.assertNotEqual(None, obj.list_nullable_struct[1])
        obj.map_nullable_float = dict([(0, None), (1, 3.14)])
        self.assertEqual(None, obj.map_nullable_float[0])
        self.assertNotEqual(None, obj.map_nullable_float[1])
        obj.vector_nullable_string = [None, "str"]
        self.assertEqual(None, obj.vector_nullable_string[0])
        self.assertNotEqual(None, obj.vector_nullable_string[1])

    def initNestedContainers(self, obj):
        obj.lvls  = random_list(\
                        functools.partial(random_list, \
                                          functools.partial(random_list, \
                                                            random_string)))
        obj.vlSLS = random_list(\
                        functools.partial(random_list, \
                                          self.randomSimpleContainers))

        obj.vvNS = random_list(\
                        functools.partial(random_list, \
                                          test.NestedWithBase))

        obj.vss   = random_list(\
                        functools.partial(random_set, \
                                          random_string))
        obj.vmds  = random_list(\
                        functools.partial(random_map, \
                                          random.random, \
                                          random_string))
        obj.lb = random_list(random_blob)
        obj.vb = random_list(random_blob)
        obj.msb = random_map(random_string, random_blob)

    def initGeneric(self, obj):
        self.initSimpleStruct(obj)
        self.initSimpleStruct(obj.x)
        obj.z = self.randomSimpleStruct()
        obj.l = random_list(self.randomSimpleStruct)

    def serialization(self, obj, init):
        obj_type = type(obj)
        for i in range(0, 50):
            init(obj)
            new_obj = obj_type()
            self.assertFalse(obj == new_obj)
            new_obj = serialize_deserialize(obj)
            self.assertTrue(obj == new_obj)

    def marshaling(self, obj, init):
        obj_type = type(obj)
        for i in range(0, 50):
            init(obj)
            new_obj = obj_type()
            self.assertFalse(obj == new_obj)
            new_obj = marshal_unmarshal(obj)
            self.assertTrue(obj == new_obj)

    def list_operations(self, a):
        self.assertTrue(len(a) != 0)
        b = [a[i] for i in range(0, len(a))]
        self.assertTrue(len(a)==len(b) and all(a[i] == b[i] for i in range(0, len(a))))
        a.append(a[0])
        self.assertEqual(a[0], a[-1])
        del a[-1]
        self.assertTrue(len(a)==len(b) and all(a[i] == b[i] for i in range(0, len(a))))
        a.extend(b)
        self.assertTrue(all(a[i] == a[i+len(a)//2] for i in range(0, len(a)//2)))
        del a[0:len(b)]
        self.assertTrue(len(a)==len(b) and all(a[i] == b[i] for i in range(0, len(a))))
        self.assertEqual(len(a), len(b))
        a[:] = [b[0]]*len(a)
        self.assertEqual(len(a), len(b))
        self.assertTrue(all(a[i] == b[0] for i in range(0, len(a))))
        a[0:len(a)//2] = b[0]
        self.assertEqual(a[0], b[0])
        x = a[-1]
        del a[:-1]
        self.assertEqual(len(a), 1)
        self.assertTrue(a[0] == x)
        del a[:]
        self.assertTrue(len(a) == 0)

    def set_operations(self, a):
        b = list(a)
        self.assertTrue(len(a)==len(b) and all(b[i] in a for i in range(0, len(b))))
        a.discard(b[-1])
        self.assertTrue(len(a)==len(b)-1 and all(b[i] in a for i in range(0, len(b)-1)))
        self.assertFalse(b[-1] in a)
        self.assertRaises(KeyError, a.remove, b[-1])
        s1 = set(a)
        s2 = set(b)
        self.assertTrue(s1 - s2 == set())
        a.clear()
        self.assertTrue(len(a) == 0)

    def map_operations(self, a):
        keys = [e.key() for e in a]
        self.assertTrue(all(a[e.key()] == e.data() for e in a))
        x = a[keys[0]]
        del a[keys[0]]
        self.assertEqual(len(keys) - 1, len(a))
        self.assertRaises(KeyError, a.__getitem__, keys[0])
        self.assertFalse(keys[0] in a)
        a[keys[0]] = x
        self.assertEqual(len(keys), len(a))

    def test_EnumType1(self):
        self.assertEqual(5, test.EnumType1.EnumValue1)
        self.assertEqual(-10, test.EnumType1.EnumValue3)
        self.assertEqual(0x2a, test.EnumType1.EnumValue4)
        self.assertEqual(-10, test.EnumType1.EnumValue5)

    def test_SimpleStruct(self):
        obj = test.SimpleStruct()
        self.serialization(obj, self.initSimpleStruct)
        self.marshaling(obj, self.initSimpleStruct)

    def test_SimpleWithBase(self):
        obj = test.SimpleWithBase()
        self.serialization(obj, self.initSimpleWithBase)
        self.marshaling(obj, self.initSimpleWithBase)

    def test_SimpleContainers(self):
        obj = test.SimpleContainers()
        self.serialization(obj, self.initSimpleContainers)
        self.marshaling(obj, self.initSimpleContainers)
        self.list_operations(obj.l_bool)
        self.list_operations(obj.l_uint32)
        self.list_operations(obj.l_string)
        self.list_operations(obj.v_bool)
        self.list_operations(obj.v_uint8)
        self.list_operations(obj.v_double)
        self.list_operations(obj.v_string)
        self.set_operations(obj.s_uint64)
        self.map_operations(obj.m_float_uint16)
        self.map_operations(obj.m_int8_string)

    def test_Nullable(self):
        obj = test.Nullable()
        new_obj = serialize_deserialize(obj)
        self.assertTrue(obj == new_obj)
        self.assertEqual(None, obj.nullable_list)
        self.assertEqual(None, obj.nullable_struct)
        self.assertEqual(None, obj.nullable_map)
        self.assertEqual(None, obj.nullable_string)
        self.assertEqual(None, obj.nullable_blob)
        self.assertEqual(None, obj.nullable_nullable_uint32)
        self.serialization(obj, self.initNullable)
        self.marshaling(obj, self.initNullable)
        self.list_operations(obj.list_nullable_struct)
        self.list_operations(obj.vector_nullable_string)
        self.map_operations(obj.map_nullable_float)
        with self.assertRaises(TypeError):
            obj.nullable_list = "str"
        with self.assertRaises(TypeError):
            obj.nullable_struct = "str"
        with self.assertRaises(TypeError):
            obj.nullable_map = 0
        with self.assertRaises(TypeError):
            obj.nullable_string = 1
        with self.assertRaises(TypeError):
            obj.nullable_blob = 0
        with self.assertRaises(TypeError):
            obj.nullable_nullable_uint32 = 3.14
        with self.assertRaises(OverflowError):
            obj.nullable_nullable_uint32 = -1

    def test_NestedContainers(self):
        obj = test.NestedContainers()
        self.serialization(obj, self.initNestedContainers)
        self.marshaling(obj, self.initNestedContainers)
        self.list_operations(obj.lb)
        self.list_operations(obj.vb)
        self.map_operations(obj.msb)

    def test_Generics(self):
        obj = test.Generic_unittest_SimpleStruct_()
        self.serialization(obj, self.initGeneric)
        self.marshaling(obj, self.initGeneric)

    def test_SchemaDef(self):
        schema = GetRuntimeSchema(test.NestedWithBase())
        struct = schema.structs[schema.root.struct_def]
        self.assertEqual(struct.metadata.qualified_name, "unittest.NestedWithBase")
        base = schema.structs[struct.base_def.struct_def]
        self.assertEqual(base.metadata.name, "Nested")
        field = struct.fields[1]
        self.assertEqual(field.id, 3)
        self.assertEqual(field.type.id, test.BondDataType.BT_DOUBLE)
        data = Serialize(schema)
        tmp = test.SchemaDef()
        self.assertFalse(tmp == schema)
        Deserialize(data, tmp, GetRuntimeSchema(schema))
        self.assertTrue(tmp == schema)

    def test_Nothing(self):
        obj = test.unittest_Nothing()
        self.assertEqual(obj.x, None)
        self.assertEqual(obj.e, None)
        self.assertEqual(obj.l, None)
        obj.x = random_int(16)
        obj.e = test.EnumType1.EnumValue1
        obj.l = random_list(random_string)
        self.assertNotEqual(obj.x, None)
        self.assertEqual(obj.e, test.EnumType1.EnumValue1)
        self.assertNotEqual(obj.l, None)
        data = Serialize(obj)
        tmp = test.unittest_Nothing()
        Deserialize(data, tmp)
        self.assertTrue(tmp == obj)
        obj.x = None
        obj.e = None
        obj.l = None
        self.assertEqual(obj.x, None)
        self.assertEqual(obj.e, None)
        self.assertEqual(obj.l, None)

    def test_Bonded(self):
        obj = self.randomSimpleStruct()
        src = test.Bonded()
        # initialized bonded<T> with instance of T
        src.n2 = test.bonded_unittest_SimpleStruct_(obj)
        data = Serialize(src)
        dst = test.Bonded()
        Deserialize(data, dst)
        # serialize bonded<T>
        data2 = Serialize(dst.n2)
        obj1 = test.SimpleStruct()
        # deserialize from bonded<T>
        dst.n2.Deserialize(obj1)
        self.assertTrue(obj == obj1)
        obj2 = test.SimpleStruct()
        Deserialize(data2, obj2);
        self.assertTrue(obj1 == obj2)
        # bonded<T> downcasting
        src2 = test.Nested()
        src2.n2 = self.randomSimpleWithBase()
        dst2 = test.Bonded()
        Deserialize(Serialize(src), dst2)

    def test_Polymorphism(self):
        src = test.Bonded()
        obj = self.randomSimpleWithBase()
        # Marshal and instance of SimpleWithBase and us it to
        # initialized bonded<SimpleStruct> field
        Unmarshal(Marshal(obj), src.n2)
        data = Serialize(src)
        dst = test.Bonded()
        Deserialize(data, dst)
        # downcast bonded<SimpleStruct> to bonded<SimpleWithBase>
        bonded = test.bonded_unittest_SimpleWithBase_(dst.n2)
        obj2 = test.SimpleWithBase()
        bonded.Deserialize(obj2)
        self.assertTrue(obj, obj2)

if __name__ == '__main__':
    unittest.main()

