// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Tcp
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Net.Sockets;
    using System.Threading.Tasks;

    enum FrameletType
    {
        TcpConfig = 0x4743,
        TcpHeaders = 0x5248,
        LayerData = 0x594C,
        PayloadData = 0x5444,
        ProtocolError = 0x5245,
    }

    struct Framelet
    {
        private FrameletType m_type;
        private ArraySegment<byte> m_content;

        public Framelet(FrameletType type, ArraySegment<byte> contents)
        {
            switch (type)
            {
                case FrameletType.TcpConfig:
                case FrameletType.TcpHeaders:
                case FrameletType.LayerData:
                case FrameletType.PayloadData:
                case FrameletType.ProtocolError:
                    break;

                default:
                    throw new ArgumentException("Unknown framelet type", nameof(type));
            }

            if (contents.Array == null || contents.Count == 0)
            {
                throw new ArgumentException("Framelet contents cannot be empty", nameof(contents));
            }

            m_type = type;
            m_content = contents;
        }

        public Framelet(FrameletType type, Int32 intValue)
        {
            if (type != FrameletType.ProtocolError)
            {
                throw new ArgumentException("Only the " + nameof(FrameletType.ProtocolError) + " framelet can be created from an integer.", nameof(intValue));
            }

            m_type = type;
            m_content = new ArraySegment<byte>(BitConverter.GetBytes(intValue));
        }

        public static bool IsKnownType(UInt16 value)
        {
            return value == (UInt16)FrameletType.LayerData
                | value == (UInt16)FrameletType.PayloadData
                | value == (UInt16)FrameletType.TcpHeaders
                | value == (UInt16)FrameletType.TcpConfig
                | value == (UInt16)FrameletType.ProtocolError;
        }

        public FrameletType Type { get { return m_type; } }
        public ArraySegment<byte> Contents { get { return m_content; } }
    }

    class Frame
    {
        // most frames will have at most three framelets: TcpHeaders, LayerData, PayloadData
        private const int DefaultFrameCount = 3;

        private readonly List<Framelet> m_framelets;

        public Frame() : this(DefaultFrameCount) { }

        public Frame(int capacity)
        {
            m_framelets = new List<Framelet>(capacity);
        }

        public IReadOnlyList<Framelet> Framelets { get { return m_framelets; } }

        public int Count { get { return m_framelets.Count; } }

        public void Add(Framelet framelet)
        {
            if (m_framelets.Count == UInt16.MaxValue)
            {
                throw new ArgumentException("Exceeded maximum allowed count of framelets.");
            }

            m_framelets.Add(framelet);
        }

        public void Write(BinaryWriter dest)
        {
            if (m_framelets.Count == 0)
            {
                throw new InvalidOperationException("Must have at least one framelet to write a frame.");
            }

            // Add ensures that we never have more than UInt16 framelets
            var numFramelets = unchecked((UInt16)m_framelets.Count);
            dest.Write(numFramelets);

            foreach (var framelet in m_framelets)
            {
                // Framelet ctor checks that the type is valid, so we don't need to worry about overflow
                var frameletType = unchecked((UInt16)framelet.Type);
                // ArraySegment checks that the Count is not negative, so we don't need to worry about overflow
                var frameletLength = unchecked((UInt32)framelet.Contents.Count);

                dest.Write(frameletType);
                dest.Write(frameletLength);
                dest.Write(framelet.Contents.Array, framelet.Contents.Offset, framelet.Contents.Count);
            }
        }

        public static Task<Frame> ReadAsync(NetworkStream stream)
        {
            var reader = new BinaryReader(stream, encoding: System.Text.Encoding.UTF8, leaveOpen: true);

            var frameletCount = reader.ReadUInt16();
            if (frameletCount == 0)
            {
                return TaskExt.FromException<Frame>(new ProtocolErrorException("Zero framelets"));
            }

            var frame = new Frame(frameletCount);

            while (frameletCount > 0)
            {
                var frameletType = reader.ReadUInt16();
                if (!Framelet.IsKnownType(frameletType))
                {
                    return TaskExt.FromException<Frame>(new ProtocolErrorException("Unknown framelet type: " + frameletType));
                }

                var frameletLength = reader.ReadUInt32();
                if (frameletLength > int.MaxValue)
                {
                    return TaskExt.FromException<Frame>(new ProtocolErrorException("Framelet too big: " + frameletLength));
                }

                var frameletContents = new byte[frameletLength];
                int bytesToRead = (int)frameletLength;
                while (bytesToRead > 0)
                {
                    int dataRead = reader.Read(frameletContents, (int)frameletLength - bytesToRead, bytesToRead);
                    if (dataRead == 0)
                    {
                        return TaskExt.FromException<Frame>(new ProtocolErrorException("EOS while reading contents"));
                    }

                    bytesToRead -= dataRead;
                }

                frame.Add(new Framelet((FrameletType)frameletType, new ArraySegment<byte>(frameletContents)));

                --frameletCount;
            }

            return Task.FromResult(frame);
        }
    }
}
