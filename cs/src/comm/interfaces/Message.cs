// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Diagnostics;
    using System.Linq;
    using System.Reflection;

    public static class Message
    {
        public static Message<TPayload> FromPayload<TPayload>(TPayload payload)
        {
            return FromPayload(MakeIBonded(payload));
        }

        public static Message<TPayload> FromPayload<TPayload>(IBonded<TPayload> payload)
        {
            return new Message<TPayload>(payload);
        }

        public static Message<TPayload> FromError<TPayload>(Error err)
        {
            return FromError<TPayload>(MakeIBonded(err));
        }

        public static Message<TPayload> FromError<TPayload>(IBonded<Error> err)
        {
            return new Message<TPayload>(err);
        }

        internal static IBonded<TBonded> MakeIBonded<TBonded>(TBonded payload)
        {
            var payloadBondedType = typeof (Bonded<>).MakeGenericType(payload.GetType());
            var ctor = payloadBondedType.GetTypeInfo().DeclaredConstructors.First();
            var bonded = ctor.Invoke(new object[] { payload });
            return (IBonded<TBonded>)bonded;
        }
    }

    public class Message<TPayload>
    {
        private IBonded<TPayload> m_payload;
        private IBonded<Error> m_error;

        public Message(TPayload payload) : this(Message.MakeIBonded(payload)) { }

        public Message(IBonded<TPayload> payload)
        {
            m_payload = payload;
            m_error = null;
        }

        // To create an error Message, use Message<TPayload>.FromError().
        //
        // This ctor is internal so that a non-error Message<Error> can be created. If this
        // were public, then new Message<Error>(SomeError) would resolve to this ctor, creating an
        // Error message, instead of to the generic ctor. We need new Message<Error>(SomeError) to
        // resolve to the generic ctor to create a non-error Message.
        internal Message(IBonded<Error> error)
        {
            m_payload = null;
            m_error = error;
        }

        public IBonded<TPayload> Payload
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
    }
}
