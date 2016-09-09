// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Diagnostics;

    /// <summary>
    /// Interface representing a message of an unknown payload type.
    /// </summary>
    /// <remarks>
    /// A message can contain either a payload or an error, represented by
    /// <see cref="Bond.Comm.Error"/>.
    /// </remarks>
    public interface IMessage
    {
        /// <summary>
        /// Gets the payload as a <see cref="IBonded"/> value.
        /// </summary>
        /// <exception cref="InvalidOperationException">
        /// Thrown when the message contains an error and not a payload.
        /// </exception>
        IBonded RawPayload { get; }

        /// <summary>
        /// Gets the error, if any.
        /// </summary>
        /// <remarks>
        /// If the message has a payload and no error, <c>null</c> is returned.
        /// </remarks>
        IBonded<Error> Error { get; }

        /// <summary>
        /// Gets a value indicating whether the message contains an error.
        /// </summary>
        /// <remarks>
        /// If a message does not contain an error, it contains a payload.
        /// </remarks>
        bool IsError { get; }

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
    /// Interface representing a message of specific payload type.
    /// </summary>
    /// <typeparam name="T">The type of the message payload.</typeparam>
    /// <remarks>
    /// A message can contain either a payload or an error, represented by
    /// <see cref="Bond.Comm.Error"/>.
    /// </remarks>
    public interface IMessage<out T> : IMessage
    {
        /// <summary>
        /// Gets the payload as a <see cref="IBonded{T}">IBonded&lt;T&gt;</see>
        /// value.
        /// </summary>
        /// <exception cref="InvalidOperationException">
        /// Thrown when the message contains an error and not a payload.
        /// </exception>
        IBonded<T> Payload { get; }
    }

    /// <summary>
    /// A message of an unknown payload type.
    /// </summary>
    public class Message : IMessage
    {
        private IBonded payload;
        private IBonded<Error> error;

        /// <summary>
        /// Initializes a Message with the given payload.
        /// </summary>
        /// <param name="payload">The payload for the message.</param>
        public Message(IBonded payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            this.payload = payload;
            error = null;
        }

        // To create an error Message, use Message.FromError<TPayload>() or Message.FromError().
        //
        // This ctor is internal so that a non-error Message<Error> can be created. If this were
        // public, then new Message<Error>(SomeError) would resolve to this ctor, creating an error
        // message, instead of to the generic ctor. We need new Message<Error>(SomeError) to resolve
        // to the generic ctor to create a non-error Message.
        internal Message(IBonded<Error> error)
        {
            if (error == null)
            {
                throw new ArgumentNullException(nameof(error));
            }

            payload = null;
            this.error = error;
        }

        /// <summary>
        /// Creates a message from the given payload.
        /// </summary>
        /// <param name="payload">The payload for the message.</param>
        /// <returns>A payload message of unknown payload type.</returns>
        public static IMessage FromPayload(IBonded payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            return new Message(payload);
        }

        /// <summary>
        /// Creates a message from the given payload.
        /// </summary>
        /// <typeparam name="TPayload">
        /// The type of the message payload.
        /// </typeparam>
        /// <param name="payload">The payload for the message.</param>
        /// <returns>A payload message of the given payload type.</returns>
        public static IMessage<TPayload> FromPayload<TPayload>(TPayload payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            return FromPayload(MakeMostDerivedIBonded(payload));
        }

        /// <summary>
        /// Creates a message from the given payload.
        /// </summary>
        /// <typeparam name="TPayload">
        /// The type of the message payload.
        /// </typeparam>
        /// <param name="payload">The payload for the message.</param>
        /// <returns>A payload message of the given payload type.</returns>
        public static IMessage<TPayload> FromPayload<TPayload>(IBonded<TPayload> payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            return new Message<TPayload>(payload);
        }

        /// <summary>
        /// Creates an error message from the given Error.
        /// </summary>
        /// <typeparam name="TPayload">
        /// The type of the message payload
        /// </typeparam>
        /// <param name="err">The Error for the message.</param>
        /// <returns>An error message of the given payload type.</returns>
        public static IMessage<TPayload> FromError<TPayload>(Error err)
        {
            if (err == null)
            {
                throw new ArgumentNullException(nameof(err));
            }

            return FromError<TPayload>(MakeMostDerivedIBonded(err));
        }

        /// <summary>
        /// Creates an error message from the given Error.
        /// </summary>
        /// <typeparam name="TPayload">
        /// The type of the message payload.
        /// </typeparam>
        /// <param name="err">The Error for the message.</param>
        /// <returns>An error message of the given payload type.</returns>
        public static IMessage<TPayload> FromError<TPayload>(IBonded<Error> err)
        {
            if (err == null)
            {
                throw new ArgumentNullException(nameof(err));
            }

            return new Message<TPayload>(err);
        }

        /// <summary>
        /// Creates an error message from the given error.
        /// </summary>
        /// <param name="err">The Error for the message.</param>
        /// <returns>An error message of unknown payload type.</returns>
        public static IMessage FromError(Error err)
        {
            if (err == null)
            {
                throw new ArgumentNullException(nameof(err));
            }

            return FromError(MakeMostDerivedIBonded(err));
        }

        /// <summary>
        /// Creates an error message from the given Error.
        /// </summary>
        /// <param name="err">The Error for the message.</param>
        /// <returns>An error message of unknown payload type.</returns>
        public static IMessage FromError(IBonded<Error> err)
        {
            if (err == null)
            {
                throw new ArgumentNullException(nameof(err));
            }

            return new Message(err);
        }

        internal static IBonded<TActual> MakeMostDerivedIBonded<TActual>(TActual payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            Type ibondedType = typeof (Bonded<>).MakeGenericType(payload.GetType());
            return (IBonded<TActual>) Activator.CreateInstance(ibondedType, payload);
        }

        public IBonded RawPayload
        {
            get
            {
                if (IsError)
                {
                    throw new InvalidOperationException("The Payload of this message cannot be accessed, as this message contains an error.");
                }

                Debug.Assert(payload != null);
                return payload;
            }
        }

        public IBonded<Error> Error
        {
            get
            {
                Debug.Assert((payload == null) ^ (error == null));
                return error;
            }
        }

        public bool IsError
        {
            get
            {
                Debug.Assert((payload == null) ^ (error == null));
                return error != null;
            }
        }

        public IMessage<U> Convert<U>()
        {
            if (IsError)
            {
                return FromError<U>(error);
            }
            else
            {
                return FromPayload(payload.Convert<U>());
            }
        }
    }

    /// <summary>
    /// A message of known type.
    /// </summary>
    /// <typeparam name="TPayload">The type of the message payload.</typeparam>
    public class Message<TPayload> : Message, IMessage<TPayload>
    {
        /// <summary>
        /// Creates a payload message from the given payload.
        /// </summary>
        /// <param name="payload">The message payload.</param>
        public Message(TPayload payload) : base(Message.MakeMostDerivedIBonded(payload)) { }

        /// <summary>
        /// Creates a payload message from the given payload.
        /// </summary>
        /// <param name="payload">The message payload.</param>
        public Message(IBonded<TPayload> payload) : base(payload)
        {
        }

        internal Message(IBonded<Error> error) : base(error)
        {
        }

        public IBonded<TPayload> Payload
        {
            get
            {
                return RawPayload.Convert<TPayload>();
            }
        }
    }
}
