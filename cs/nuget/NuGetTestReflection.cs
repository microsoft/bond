namespace Bond.Internal.Reflection
{
    using System;
    using System.Reflection;

    /// <summary>
    /// The minimal set of reflection methods needed to keep the NuGet tests working without having
    /// access to the Bond.Reflection internals.
    /// </summary>
    // TODO: make the internals of Bond.Reflection visible to this assembly, which should always be
    //       signed with the bond.snk in the repository. I.e., don't condition this assembly on
    //       DELAY_SIGN
    static class NugetTestHelpers
    {
        public static bool IsGenericType(this Type type)
        {
            return type.GetTypeInfo().IsGenericType;
        }

        public static PropertyInfo GetDeclaredProperty(this Type type, string name)
        {
            return type.GetTypeInfo().GetDeclaredProperty(name);
        }

        public static PropertyInfo GetDeclaredProperty(this Type type, string name, Type returnType)
        {
            var property = type.GetDeclaredProperty(name);
            return (property != null && property.PropertyType == returnType) ? property : null;
        }
    }
}
