// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Grpc
{
    using System;

    /// <summary>
    /// Interface representing a Bond message of a specific payload type.
    /// </summary>
    /// <typeparam name="T">The type of the message payload.</typeparam>
    public interface IMessage<out T>
    {
        /// <summary>
        /// Gets the payload as an <see cref="IBonded{T}">IBonded&lt;T&gt;</see>
        /// value.
        /// </summary>
        IBonded<T> Payload { get; }

        /// <summary>
        /// Converts this message to an <see cref="IMessage{T}">IMessage&lt;U&gt;</see>.
        /// </summary>
        /// <typeparam name="U">The new type of the message payload.</typeparam>
        /// <returns>
        /// An instance of <see cref="IMessage{T}">IMessage&lt;U&gt;</see>. If the conversion fails,
        /// <c>null</c> is returned.
        /// </returns>
        IMessage<U> Convert<U>();
    }

    /// <summary>
    /// Factory methods for creating <see cref="IMessage{T}" /> instances.
    /// </summary>
    public static class Message
    {
        /// <summary>
        /// A static instance of the void (empty) message.
        /// </summary>
        public static readonly IMessage<Bond.Void> Void = From((IBonded<Bond.Void>)Bonded<Bond.Void>.Empty);

        /// <summary>
        /// Creates a message from the given object.
        /// </summary>
        /// <typeparam name="TPayload">
        /// The type of the message payload.
        /// </typeparam>
        /// <param name="payload">The payload for the message.</param>
        /// <returns>A message of the given payload type.</returns>
        public static IMessage<TPayload> From<TPayload>(TPayload payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            return From(MakeMostDerivedIBonded(payload));
        }

        /// <summary>
        /// Creates a message from the given <see cref="IBonded{T}">IBonded instance</see>.
        /// </summary>
        /// <typeparam name="TPayload">
        /// The type of the message payload.
        /// </typeparam>
        /// <param name="payload">The payload for the message.</param>
        /// <returns>A message of the given payload type.</returns>
        public static IMessage<TPayload> From<TPayload>(IBonded<TPayload> payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            return new Message<TPayload>(payload);
        }

        /// <summary>
        /// Creates an <see cref="IBonded{T}">IBonded&lt;T&gt;</see> instance backed by <paramref name="payload"/>,
        /// where <c>T</c> is determined by the actual runtime type of <paramref name="payload"/>. This helps
        /// avoid unintended slicing to <typeparamref name="TMessage"/> if <paramref name="payload"/> is derived from
        /// <typeparamref name="TMessage"/> instead of being a <typeparamref name="TMessage"/>.
        /// </summary>
        /// <typeparam name="TMessage">The type of the message to create.</typeparam>
        /// <param name="payload">The payload object instance.</param>
        /// <returns>A IMessage&lt;TMessage&gt; backed by <paramref name="payload"/> that will not slice.</returns>
        internal static IBonded<TMessage> MakeMostDerivedIBonded<TMessage>(TMessage payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            Type ibondedType = typeof (Bonded<>).MakeGenericType(payload.GetType());
            return (IBonded<TMessage>) Activator.CreateInstance(ibondedType, payload);
        }
    }

    /// <summary>
    /// A Bond message of a given type
    /// </summary>
    /// <typeparam name="TPayload">The type of the message payload.</typeparam>
    public class Message<TPayload> : IMessage<TPayload>
    {
        /// <summary>
        /// Creates a message from the given <see cref="IBonded{T}">IBonded instance</see>.
        /// </summary>
        /// <param name="payload">The message payload.</param>
        public Message(IBonded<TPayload> payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            Payload = payload;
        }

        /// <summary>
        /// Creates a message from the given object.
        /// </summary>
        /// <param name="payload">The message payload.</param>
        public Message(TPayload payload) : this(Message.MakeMostDerivedIBonded(payload)) { }

        /// <summary>
        /// Gets the payload of this message.
        /// </summary>
        public IBonded<TPayload> Payload { get; }

        /// <summary>
        /// Convert to an instance of <see cref="IMessage{T}">IMessage&lt;U&gt;</see>
        /// </summary>
        /// <typeparam name="U">Type representing a Bond schema</typeparam>
        /// <returns>An instance of IMessage&lt;U&gt;. If the conversion fails,
        /// <c>null</c> is returned.</returns>
        public IMessage<U> Convert<U>()
        {
            IBonded<U> bonded = Payload.Convert<U>();
            if (bonded == null)
            {
                return null;
            }

            return Message.From(bonded);
        }
    }
}
