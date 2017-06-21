namespace UnitTest
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Reflection;
    using Bond;
    using Bond.Internal.Reflection;

    public static class Equal
    {
        static readonly MethodInfo comparerEqual = Util.GetMethod(typeof(Comparer), "Equal");

        public static bool IsEqual<T1, T2>(this T1 left, T2 right)
            where T1 : class
            where T2 : class
        {
            // C# generics can't infer overloads so we have to do do this manually.
            var eLeft = left as System.Collections.IEnumerable;
            var eRight = right as System.Collections.IEnumerable;

            if (eLeft != null && eRight != null)
            {
                var e1 = eLeft.GetEnumerator();
                var e2 = eRight.GetEnumerator();
                bool n1 = e1.MoveNext(), n2 = e2.MoveNext();
                for (; n1 && n2; n1 = e1.MoveNext(), n2 = e2.MoveNext())
                {
                    if (!IsEqual<object, object>(e1.Current, e2.Current))
                        return false;
                }

                return !n1 && !n2;
            }

            if (left.GetType().IsGenericType())
            {
                if (left.GetType().GetGenericTypeDefinition() == typeof(KeyValuePair<,>))
                {
                    var leftKey = left.GetType().GetProperty("Key");
                    var leftValue = left.GetType().GetProperty("Value");
                    var rightKey = right.GetType().GetProperty("Key");
                    var rightValue = right.GetType().GetProperty("Value");

                    return IsEqual<object, object>(leftKey.GetValue(left, null), rightKey.GetValue(right, null)) &&
                           IsEqual<object, object>(leftValue.GetValue(left, null), rightValue.GetValue(right, null));
                }
            }

            if (right.GetType() == typeof(double) && left.GetType() == typeof(float))
            {
                return (float)(object)(left) == (float)(double)(object)(right);
            }

            // handle comparison of different but compatible numeric types
            var equal = right.GetType().GetMethod("Equals", right.GetType());
            if (equal != null)
                return (bool)equal.Invoke(right, new object[] { left });

            if (left.GetType() == right.GetType())
                return left.IsEqual<T1>(right as T1);

            if (left.GetType() == typeof(Optional) && right.GetType() == typeof(RequiredOptional))
                return IsEqual(left as Optional, right as RequiredOptional);

            if (left.GetType() == typeof(BasicTypes) && right.GetType() == typeof(BasicTypesView))
                return IsEqual(left as BasicTypes, right as BasicTypesView);

            if (left.GetType() == typeof(GenericWString) && right.GetType() == typeof(NonGenericWString))
                return IsEqual(left as GenericWString, right as NonGenericWString);

            if (left.GetType() == typeof(NotNothingView) && right.GetType() == typeof(Nothing))
                return IsEqual(left as NotNothingView, right as Nothing);

            if (left.GetType() == typeof(Derived) && right.GetType() == typeof(DerivedView))
                return IsEqual(left as Derived, right as DerivedView);

            if (left.GetType() == typeof(DerivedView) && right.GetType() == typeof(Derived))
                return IsEqual(right as Derived, left as DerivedView);

            if (left.GetType() == typeof(StructWithBlobs) && right.GetType() == typeof(StructWithByteLists))
                return IsEqual(left as StructWithBlobs, right as StructWithByteLists);

            if (left.GetType() == typeof(StructWithByteLists) && right.GetType() == typeof(StructWithBlobs))
                return IsEqual(right as StructWithBlobs, left as StructWithByteLists);

            if (left.GetType() == typeof(Derived) || left.GetType() == typeof(DerivedView))
            {
                if (right.GetType() == typeof (EmptyBase))
                    return IsEqual<EmptyBase>(left as EmptyBase, right as EmptyBase);

                if (right.GetType() == typeof(Nested))
                    return IsEqual<Nested>(left as Nested, right as Nested);
            }

            if (left.GetType() == typeof(EmptyBase) && right.GetType() == typeof(Nested))
                return IsEqual<Nested>(left as Nested, left as Nested);

            return left.Equals(right);
        }

        public static bool IsEqual(this BasicTypes b, BasicTypesView v)
        {
            return v._bool == b._bool &&
                   v._float == b._float &&
                   v._int8 == b._int8;
        }

        public static bool IsEqual(this Optional o, RequiredOptional ro)
        {
            return o.x == ro.x;
        }

        public static bool IsEqual(this NotNothingView v, Nothing n)
        {
            return v._float == n._float &&
                   v._enum1 == n._enum1 &&
                   Comparer.Equal(v.l, n.l) &&
                   Comparer.Equal(v.s, n.s) &&
                   Comparer.Equal(v.vl, n.vl);
        }

        public static bool IsEqual(this Derived d, DerivedView v)
        {
            return v.derived == d.derived &&
                   Comparer.Equal<EmptyBase>(d, v);
        }

        public static bool IsEqual(this GenericWString g, NonGenericWString n)
        {
            return g.wstr.value == n.wstr.value;
        }

        public static bool IsEqual(this StructWithBlobs b, StructWithByteLists v)
        {
            if (b.lb.Count != v.lb.Count)
                return false;

            for (var i = 0; i < b.lb.Count; ++i)
                if (!IsEqual(b.lb[i], v.lb[i]))
                    return false;
            
            return IsEqual(b.b, v.b) &&
                ((b.nb.Array == null) && (v.nb == null) || IsEqual(b.nb, v.nb));
        }

        static bool IsEqual<T>(ArraySegment<byte> left, T right)
            where T : ICollection<sbyte>
        {
            if (left.Count != right.Count)
                return false;

            var i = left.Offset;
            return right.All(b => left.Array[i++] == (byte) b);
        }

        public static bool IsEqual<T>(this T left, T right) where T : class
        {
            var equal = comparerEqual.MakeGenericMethod(right.GetType());
            var result = (bool)equal.Invoke(null, new object[] { left, right });

            // Uncomment this block and set breakpoint here to debug unequal test failure
            //if (!result)
            //{
            //    var strLeft = Util.SerializeXmlString(left);
            //    var strRight = Util.SerializeXmlString(right);
            //}

            return result;
        }
    }
}