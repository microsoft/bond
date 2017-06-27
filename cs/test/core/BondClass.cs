namespace UnitTest
{
    using System.Collections.Generic;
    using System.Reflection;
    using Bond;

    [Schema, Attribute("xmlns", "urn:UnitTest.BondClass")]
    class BondClass<T>
    {
        [Bond.Id(2), Required]
        public T field = GenericFactory.Create<T>();

        public static BondDataType TypeId 
        { 
            get { return Schema<BondClass<T>>.RuntimeSchema.StructDef.fields[0].type.id; }
        }

        public override bool Equals(object that)
        {
            var thatField = that.GetType().GetTypeInfo().GetDeclaredField("field");
            return field.IsEqual<object, object>(thatField.GetValue(that));
        }
            
        public override int GetHashCode()
        {
            return EqualityComparer<T>.Default.GetHashCode(field);
        }
    }

    [Schema, Attribute("xmlns", "urn:UnitTest.BondClass")]
    internal class BondClass<T1, T2>
    {
        [Bond.Id(1), Required]
        public T1 extra = GenericFactory.Create<T1>();

        [Bond.Id(2), Required]
        public T2 field = GenericFactory.Create<T2>();

        public override bool Equals(object that)
        {
            if (that is BondClass<T2>)
            {
                var thatField = that.GetType().GetTypeInfo().GetDeclaredField("field");
                return field.IsEqual<object, object>(thatField.GetValue(that));
            }

            return false;
        }
        public override int GetHashCode()
        {
            return EqualityComparer<T2>.Default.GetHashCode(field);
        }
    }
}
