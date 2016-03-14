// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Diagnostics;

    public class Message<TPayload>
    {
        public Message(TPayload payload) : this(new Bonded<TPayload>(payload)) { }

        public Message(IBonded<TPayload> payload)
        {
            m_payload = payload;
            m_error = null;
        }

        public Message(Error error) : this(new Bonded<Error>(error)) { }

        public Message(IBonded<Error> error)
        {
            m_payload = null;
            m_error = error;
        }

        private IBonded<TPayload> m_payload;
        // Could use bool and have one IBonded field for both. Let's let perf tests show the way.
        private IBonded<Error> m_error;

        // Should there be some way to replace the payload with a live
        // object if, say, a layer need to fully deserializes the
        // payload?
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
