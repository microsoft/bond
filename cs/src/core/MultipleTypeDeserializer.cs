// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Collections.Generic;
    using System.Linq.Expressions;
    using Bond.Expressions;

    /// <summary>
    /// MultiTypeDeserializer is used to deserialize more than one type from protocol R, allowing compiled
    /// delegates to be reused across types.
    /// </summary>
    /// <typeparam name="R">Protocol reader type</typeparam>
    public class MultipleTypeDeserializer<R>
    {
        readonly IFactory objectFactory; 

        readonly Dictionary<Type, Func<R, object>> compiledDeserializeFuncs = new Dictionary<Type, Func<R, object>>();
        readonly object compiledFuncsSync = new object();

        readonly Dictionary<Type, DeserializerTransform<R>.TypeState> sharedTypeStates = 
            new Dictionary<Type, DeserializerTransform<R>.TypeState>();
        readonly object typeStatesSync = new object();

        Func<R, object>[] deferredDeserializeFuncs = new Func<R, object>[50];

        public MultipleTypeDeserializer(IFactory objectFactory)
        {
            this.objectFactory = objectFactory;
        }

        public MultipleTypeDeserializer()
        {
        }

        public void AddType(Type type, RuntimeSchema schema)
        {
            AddType(type, ParserFactory<R>.Create(schema));
        }

        public void AddType(Type type)
        {
            AddType(type, ParserFactory<R>.Create(type));
        }

        private void AddType(Type type, IParser parser)
        {
            lock (compiledFuncsSync)
            {
                if (compiledDeserializeFuncs.ContainsKey(type)) return;
            }

            var transform = objectFactory == null
                ? new DeserializerTransform<R>(
                    HandleDeserializeExpression,
                    (r, i) => deferredDeserializeFuncs[i](r), 
                    null, 
                    null,
                    sharedTypeStates,
                    typeStatesSync,
                    noInlining: true)
                : new DeserializerTransform<R>(
                    HandleDeserializeExpression,
                    (r, i) => deferredDeserializeFuncs[i](r),
                    (t1, t2) => objectFactory.CreateObject(t1, t2),
                    (t1, t2, count) => objectFactory.CreateContainer(t1, t2, count),
                    sharedTypeStates,
                    typeStatesSync,
                    noInlining: true);

            transform.Generate(parser, type);
        }

        private void HandleDeserializeExpression(Expression<Func<R, object>> expression, Type objectType, int index)
        {
            Func<R, object> func;
            if (deferredDeserializeFuncs.Length > index && deferredDeserializeFuncs[index] != null)
            {
                // We already have a func for this type, ignore.
            }

            func = expression.Compile();
            lock (compiledFuncsSync)
            {
                compiledDeserializeFuncs[objectType] = func;
            }

            // Resize deferred array if needed
            var length = deferredDeserializeFuncs.Length;
            if (length <= index)
            {
                var newLength = Math.Max(length*2, index + 1);
                var newDeferred = new Func<R, object>[newLength];
                Array.Copy(deferredDeserializeFuncs, newDeferred, length);

                // Ensure an atomic operation puts in place a valid resized array. 
                // Calling Array.Resize might create an interim state where the deferredDeserializeFunc array does 
                // not have all the content.
                deferredDeserializeFuncs = newDeferred;
            }
            deferredDeserializeFuncs[index] = func;
        }

        /// <summary>
        /// Deserialize an object of type T from a payload
        /// </summary>
        /// <typeparam name="T">Type representing a Bond schema</typeparam>
        /// <param name="reader">Protocol reader representing the payload</param>
        /// <returns>Deserialized object</returns>
        public T Deserialize<T>(R reader)
        {
            return (T)Deserialize(reader, typeof(T));
        }

        public object Deserialize(R reader, Type type)
        {
            Func<R, object> func;
            lock (compiledFuncsSync)
            {
                compiledDeserializeFuncs.TryGetValue(type, out func);
            }

            if (func == null)
            {
                AddType(type);
            }

            lock (compiledFuncsSync)
            {
                func = compiledDeserializeFuncs[type];
            }

            return func(reader);
        }
    }
}
