// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System.Collections.Generic;
    using System.Linq.Expressions;

    /// <summary>
    /// Handler for containers
    /// </summary>
    /// <param name="valueParser">Parser instance that can be used to process the elements</param>
    /// <param name="valueType">Expression of type BondDataType indicating the element type</param>
    /// <param name="next">Expression of type bool indicating if there are more elements</param>
    /// <param name="count">Expression of type int indicating number of elements, may be null</param>
    /// <param name="arraySegment">Parameter expression of type ArraySegment that holds conveted input bytes, may be null</param>
    /// <returns>Expression to handle the container</returns>
    public delegate Expression ContainerHandler(
        IParser valueParser, Expression valueType, Expression next, Expression count, ParameterExpression arraySegment);
    
    /// <summary>
    /// Handler for maps
    /// </summary>
    /// <param name="keyParser">Parser instance that can be used to process the keys</param>
    /// <param name="valueParser">Parser instance that can be used to process the values</param>
    /// <param name="keyType">Expression of type BondDataType indicating the key type</param>
    /// <param name="valueType">Expression of type BondDataType indicating the value type</param>
    /// <param name="nextKey">Expression of type bool indicating if there are more keys</param>
    /// <param name="nextValue">Expression of type bool indicating if there are more values</param>
    /// <param name="count">Expression of type int indicating number of key/value pairs, may be null</param>
    /// <returns>Expression to handle the map</returns>
    public delegate Expression MapHandler(
        IParser keyParser, IParser valueParser, Expression keyType, Expression valueType, 
        Expression nextKey, Expression nextValue, Expression count);
    
    /// <summary>
    /// Handler of scalar values
    /// </summary>
    /// <param name="value">Expression representing the value</param>
    /// <returns>Expression to handle the value</returns>
    public delegate Expression ValueHandler(Expression value);

    /// <summary>
    /// Interface implemented by transforms to handle fields
    /// </summary>
    public interface IField
    {
        /// <summary>
        /// Identifier (a.k.a. ordinal) of the field
        /// </summary>
        ushort Id { get; }
        
        /// <summary>
        /// Called by parser to process the field value
        /// </summary>
        /// <param name="parser">Parser instance that can be used to process the field</param>
        /// <param name="valueType">Expression of type BondDataType indicating the field type</param>
        /// <returns>Expression to handle the field</returns>
        Expression Value(IParser parser, Expression valueType);

        /// <summary>
        /// Expression used by parser for fields that have default value and were/can be omitted
        /// </summary>
        /// <value>Expression to handle the omitted field</value>
        Expression Omitted { get; }
    }

    /// <summary>
    /// Interface implemented by transforms to process Bond schema instances
    /// </summary>
    public interface ITransform
    {
        /// <summary>
        /// First expression used by the parser when a transform is applied
        /// </summary>
        /// <value>Expression starting transform</value>
        Expression Begin { get; }

        /// <summary>
        /// Called by the parser for the immediate base schema
        /// </summary>
        /// <param name="parser">Parser instance that can be used to process the base schema, 
        /// usually by applying a transform to it</param>
        /// <returns>Expression to handle the base</returns>
        Expression Base(IParser parser);
        
        /// <summary>
        /// List of fields, sorted in order of increasing ordinals, that the transform can process
        /// </summary>
        IEnumerable<IField> Fields { get; }
        
        /// <summary>
        /// Called by the parser for fields that are not found in the <see cref="Fields"/> list
        /// </summary>
        /// <param name="parser">Parser instance that can be used to process the field value</param>
        /// <param name="fieldType">Expression of type BondDataType representing the field type</param>
        /// <param name="fieldId">Expression of type ushort representing the field ordinal</param>
        /// <returns>Expression to handle the unknown field</returns>
        Expression UnknownField(IParser parser, Expression fieldType, Expression fieldId);

        /// <summary>
        /// Expression used by the parser for an end of unknown base schema (i.e. base for which there
        /// was no transform applied)
        /// </summary>
        /// <value>Expression to handle the unknown schema end</value>
        Expression UnknownEnd { get; }

        /// <summary>
        /// Last expression used by the parser for the transform
        /// </summary>
        /// <value>Expression ending transform</value>
        Expression End { get; }
    }

    /// <summary>
    /// Interface allowing parsing of different representations of Bond schema instances, 
    /// such as serialized payloads or objects.
    /// </summary>
    public interface IParser
    {
        /// <summary>
        /// Apply transform to Bond schema instance represented by the parser
        /// </summary>
        /// <param name="transform"></param>
        /// <returns>Expression to process the schema instance</returns>
        Expression Apply(ITransform transform);

        /// <summary>
        /// Handle a container given a parser instance representing a BT_LIST or BT_SET
        /// </summary>
        /// <param name="expectedType">Optional type of element expected by the caller</param>
        /// <param name="handler">Handler processing the container</param>
        /// <returns>Expression to process the container</returns>
        Expression Container(BondDataType? expectedType, ContainerHandler handler);

        /// <summary>
        /// Handle a container given a parser instance representing a BT_MAP
        /// </summary>
        /// <param name="expectedKeyType">Optional type of key expected by the caller</param>
        /// <param name="expectedValueType">Optional type of value expected by the caller</param>
        /// <param name="handler">Handler processing the map</param>
        /// <returns>Expression to process the map</returns>
        Expression Map(BondDataType? expectedKeyType, BondDataType? expectedValueType, MapHandler handler);

        /// <summary>
        /// Get the content of the container as an ArraySegment&lt;byte>
        /// </summary>
        /// <param name="count">Number of bytes in the container</param>
        /// <returns>Expression of type ArraySegment&lt;byte> or null if the parser can't provide the content 
        /// as an ArraySegment&lt;byte></returns>
        Expression Blob(Expression count);

        /// <summary>
        /// Handle a scalar value
        /// </summary>
        /// <param name="valueType">Expression of type BondDataType indicating the parser value type</param>
        /// <param name="expectedType">Type of value expected by the caller</param>
        /// <param name="handler">Handler processing the value</param>
        /// <returns>Expression to process the value</returns>
        Expression Scalar(Expression valueType, BondDataType expectedType, ValueHandler handler);

        /// <summary>
        /// Get the value as an instance of <see cref="IBonded"/>, can be called for value of type BT_STRUCT 
        /// and must be called to get value if <see cref="IsBonded"/> returns true
        /// </summary>
        /// <param name="handler">Handler processing the IBonded instance</param>
        /// <returns>Expression to process the IBonded value</returns>
        Expression Bonded(ValueHandler handler);

        /// <summary>
        /// Skip a value of specified type
        /// </summary>
        /// <param name="valueType">Expression of type BondDataType indicating the type to skip</param>
        /// <returns>Expression to skip the value</returns>
        Expression Skip(Expression valueType);

        /// <summary>
        /// Expression representing variable holding the Reader used by the parser
        /// </summary>
        ParameterExpression ReaderParam { get; }

        /// <summary>
        /// Expression representing value of the Reader used by the parser
        /// </summary>
        Expression ReaderValue { get; }

        /// <summary>
        /// Depth of the schema hierarchy represented by the parser, may be 0 if unknown
        /// </summary>
        int HierarchyDepth { get; }

        /// <summary>
        /// Indicates whether the value represented by the parser must be handled as IBonded
        /// </summary>
        bool IsBonded { get; }
    }
}
