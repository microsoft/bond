// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;

    // Tag types used to annotate Bond schema types via Type attribute
    // ReSharper disable InconsistentNaming
    // ReSharper disable UnusedTypeParameter
    namespace Tag
    {
        /// <summary>
        /// Represents wstring Bond schema type
        /// </summary>
        public abstract class wstring { }

        /// <summary>
        /// Represents blob Bond schema type
        /// </summary>
        public abstract class blob { }

        /// <summary>
        /// Represents nullable&lt;T> Bond schema type
        /// </summary>
        public abstract class nullable<T> { }

        /// <summary>
        /// Represents bonded&lt;T> Bond schema type
        /// </summary>
        public abstract class bonded<T> { }

        /// <summary>
        /// Represents a type parameter constrained to value types
        /// </summary>
        public struct structT { }

        /// <summary>
        /// Represents unconstrained type parameter
        /// </summary>
        public abstract class classT { }
    }
    // ReSharper restore InconsistentNaming
    // ReSharper restore UnusedTypeParameter

    /// <summary>
    /// Specifies that a type represents Bond schema
    /// </summary>
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Interface,
        Inherited = false)]
    public sealed class SchemaAttribute : Attribute
    {
    }

    /// <summary>
    /// Specifies namespace of the schema, required only if different than C# namespace of the class
    /// </summary>
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Interface | AttributeTargets.Enum,
        Inherited = false)]
    public sealed class NamespaceAttribute : Attribute
    {
        public NamespaceAttribute(string value)
        {
            Value = value;
        }

        internal string Value { get; private set; }
    }

    /// <summary>
    /// Specifies field identifier. Required for all Bond fields
    /// </summary>
    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, Inherited = false)]
    public sealed class IdAttribute : Attribute
    {
        public IdAttribute(ushort id)
        {
            Value = id;
        }

        internal ushort Value { get; private set; }
    }

    /// <summary>
    /// Specifies type of a field in Bond schema
    /// </summary>
    /// <remarks>
    /// If absent the type is inferred from C# type using the following rules:
    /// - numeric types map in the obvious way
    /// - IList, ICollection -> BT_LIST
    /// - IDictionary -> BT_MAP
    /// - ISet -> BT_SET
    /// - string -> BT_STRING (Utf8)
    /// - Bond struct fields initialized to null map to nullable
    /// - other fields initialized to null map to default of nothing
    /// The Type attribute is necessary in the following cases:
    /// - nullable types, e.g. Type[typeof(list&lt;nullable&lt;string>>)]
    /// - Utf16 string (BT_WSTRING), e.g. Type[typeof(wstring)]
    /// - user specified type aliases
    /// </remarks>
    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, Inherited = false)]
    public sealed class TypeAttribute : Attribute
    {
        public TypeAttribute(Type type)
        {
            Value = type;
        }

        internal Type Value { get; private set; }
    }

    /// <summary>
    /// Specifies the default value of a field
    /// </summary>
    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, Inherited = false)]
    public sealed class DefaultAttribute : Attribute
    {
        public DefaultAttribute(object value)
        {
            Value = value;
        }

        public object Value { get; private set; }
    }

    /// <summary>
    /// Specifies that a field is required
    /// </summary>
    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, Inherited = false)]
    public sealed class RequiredAttribute : Attribute
    {
    }

    /// <summary>
    /// Specifies that a field is required_optional
    /// </summary>
    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, Inherited = false)]
    public sealed class RequiredOptionalAttribute : Attribute
    {
    }

    /// <summary>
    /// Specifies user defined schema attribute
    /// </summary>
    [AttributeUsage(
        AttributeTargets.Class |
        AttributeTargets.Enum |
        AttributeTargets.Field |
        AttributeTargets.Interface |
        AttributeTargets.Method |
        AttributeTargets.Property |
        AttributeTargets.Struct ,
        AllowMultiple = true, Inherited = false)]
    public sealed class AttributeAttribute : Attribute
    {
        public AttributeAttribute(string name, string value)
        {
            Name = name;
            Value = value;
        }

        public string Name { get; private set; }

        public string Value { get; private set; }
    }

    /// <summary>
    /// Applied to protocol readers to indicate the IParser implementation used for parsing the protocol
    /// </summary>
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Interface)]
    public sealed class ParserAttribute : Attribute
    {
        public ParserAttribute(Type parserType)
        {
            ParserType = parserType;
        }

        internal Type ParserType { get; private set; }
    }

    /// <summary>
    /// Applied to protocol writers to indicate the reader type for the protocol
    /// </summary>
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Interface)]
    public sealed class ReaderAttribute : Attribute
    {
        public ReaderAttribute(Type readerType)
        {
            ReaderType = readerType;
        }

        internal Type ReaderType { get; private set; }
    }

    /// <summary>
    /// Applied to protocol writers to indicate the implementation of ISerializerGenerator used to generate serializer
    /// </summary>
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Interface)]
    public sealed class SerializerAttribute : Attribute
    {
        public SerializerAttribute(Type type)
        {
            Type = type;
        }

        internal Type Type { get; private set; }
    }

    /// <summary>
    /// Applied to 2-pass protocol writers to indicate the implementation of IProtocolWriter used to generate the first-pass serializer
    /// </summary>
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Interface)]
    public sealed class FirstPassWriterAttribute : Attribute
    {
        public FirstPassWriterAttribute(Type type)
        {
            Type = type;
        }

        internal Type Type { get; private set; }
    }
}
