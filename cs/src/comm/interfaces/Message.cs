// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Diagnostics;
    using System.Linq;
    using System.Reflection;

    public interface IMessage
    {
        IBonded RawPayload { get; }
        IBonded<Error> Error { get; }
        bool IsError { get; }

        IMessage<U> Convert<U>();

    }

    public interface IMessage<out T> : IMessage
    {
        IBonded<T> Payload { get; }
    }

    public class Message : IMessage
    {
        private IBonded m_payload;
        private IBonded<Error> m_error;

        public Message(IBonded payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            m_payload = payload;
            m_error = null;
        }

        // To create an error Message, use Message.FromError<TPayload>() or Message.FromError().
        //
        // This ctor is internal so that a non-error Message<Error> can be created. If this
        // were public, then new Message<Error>(SomeError) would resolve to this ctor, creating an
        // Error message, instead of to the generic ctor. We need new Message<Error>(SomeError) to
        // resolve to the generic ctor to create a non-error Message.
        internal Message(IBonded<Error> error)
        {
            if (error == null)
            {
                throw new ArgumentNullException(nameof(error));
            }

            m_payload = null;
            m_error = error;
        }

        public static Message FromPayload(IBonded payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            return new Message(payload);
        }

        public static Message<TPayload> FromPayload<TPayload>(TPayload payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            return FromPayload(MakeIBonded(payload));
        }

        public static Message<TPayload> FromPayload<TPayload>(IBonded<TPayload> payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            return new Message<TPayload>(payload);
        }

        public static Message<TPayload> FromError<TPayload>(Error err)
        {
            if (err == null)
            {
                throw new ArgumentNullException(nameof(err));
            }

            if (err.error_code == (int)ErrorCode.OK)
            {
                throw new ArgumentException("Error must have a non-zero error code.", nameof(err));
            }

            return FromError<TPayload>(MakeIBonded(err));
        }

        public static Message<TPayload> FromError<TPayload>(IBonded<Error> err)
        {
            if (err == null)
            {
                throw new ArgumentNullException(nameof(err));
            }

            return new Message<TPayload>(err);
        }

        public static Message FromError(Error err)
        {
            if (err == null)
            {
                throw new ArgumentNullException(nameof(err));
            }

            if (err.error_code == (int)ErrorCode.OK)
            {
                throw new ArgumentException("Error must have a non-zero error code.", nameof(err));
            }

            return FromError(MakeIBonded(err));
        }

        public static Message FromError(IBonded<Error> err)
        {
            if (err == null)
            {
                throw new ArgumentNullException(nameof(err));
            }

            // can't check that err has a non-zero error code without deserializaing, so we skip that
            return new Message(err);
        }

        internal static IBonded<TActual> MakeIBonded<TActual>(TActual payload)
        {
            if (payload == null)
            {
                throw new ArgumentNullException(nameof(payload));
            }

            var payloadBondedType = typeof (Bonded<>).MakeGenericType(payload.GetType());
            var ctor = payloadBondedType.GetTypeInfo().DeclaredConstructors.First();
            var bonded = ctor.Invoke(new object[] { payload });
            return (IBonded<TActual>)bonded;
        }

        public IBonded RawPayload
        {
            get
            {
                if (IsError)
                {
                    throw new InvalidOperationException("The Payload of this message cannot be accessed, as this message contains an Error.");
                }

                Debug.Assert(m_payload != null);
                return m_payload;
            }
        }

        public IBonded<Error> Error
        {
            get
            {
                Debug.Assert((m_payload == null) ^ (m_error == null));
                return m_error;
            }
        }

        public bool IsError
        {
            get
            {
                Debug.Assert((m_payload == null) ^ (m_error == null));
                return m_error != null;
            }
        }

        public IMessage<U> Convert<U>()
        {
            if (IsError)
            {
                return FromError<U>(m_error);
            }
            else
            {
                return FromPayload(m_payload.Convert<U>());
            }
        }
    }

    public class Message<TPayload> : Message, IMessage<TPayload>
    {
        public Message(TPayload payload) : base(Message.MakeIBonded(payload)) { }

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
