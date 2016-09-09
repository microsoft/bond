// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.IO;
    using System.Text;
    using System.Threading;
    using System.Threading.Tasks;

    internal enum FrameletType
    {
        EpoxyConfig = 0x4743,           // "GC"
        EpoxyHeaders = 0x5248,          // "RH"
        ErrorData = 0x4445,             // "DE"
        LayerData = 0x594C,             // "YL"
        PayloadData = 0x4450,           // "DP"
        ProtocolError = 0x5245,         // "RE"
    }

    internal struct Framelet
    {
        public Framelet(FrameletType type, ArraySegment<byte> contents)
        {
            switch (type)
            {
                case FrameletType.EpoxyConfig:
                case FrameletType.EpoxyHeaders:
                case FrameletType.ErrorData:
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

            Type = type;
            Contents = contents;
        }

        public static bool IsKnownType(UInt16 value)
        {
            return value == (UInt16) FrameletType.EpoxyConfig
                   || value == (UInt16) FrameletType.EpoxyHeaders
                   || value == (UInt16) FrameletType.ErrorData
                   || value == (UInt16) FrameletType.LayerData
                   || value == (UInt16) FrameletType.PayloadData
                   || value == (UInt16) FrameletType.ProtocolError;
        }

        public FrameletType Type { get; }
        public ArraySegment<byte> Contents { get; }
    }

    internal class Frame
    {
        // most frames will have at most three framelets: EpoxyHeaders, LayerData, PayloadData/ErrorData
        private const int DefaultFrameCount = 3;

        private readonly List<Framelet> framelets;

        private readonly Logger logger;

        public Frame(Logger logger) : this(DefaultFrameCount, logger) { }

        public Frame(int capacity, Logger logger)
        {
            framelets = new List<Framelet>(capacity);

            // start with space for count of framelets
            TotalSize = sizeof (UInt16);

            this.logger = logger;
        }

        public IReadOnlyList<Framelet> Framelets => framelets;

        public int Count => framelets.Count;

        public int TotalSize { get; private set; }

        public void Add(Framelet framelet)
        {
            if (framelets.Count == UInt16.MaxValue)
            {
                var message = logger.Site().ErrorAndReturnFormatted("Exceeded maximum allowed count of framelets.");
                throw new InvalidOperationException(message);
            }

            try
            {
                // add space for framelet type, length of message, and actual message content
                TotalSize = checked(TotalSize + sizeof(UInt16) + sizeof(UInt32) + framelet.Contents.Count);
                framelets.Add(framelet);
            }
            catch (OverflowException oex)
            {
                var message = logger.Site().ErrorAndReturnFormatted("Exceeded maximum size of frame.");
                throw new InvalidOperationException(message, oex);
            }
        }

        public async Task WriteAsync(Stream stream)
        {
            Debug.Assert(stream != null);

            if (framelets.Count == 0)
            {
                throw new InvalidOperationException("Must have at least one framelet to write a frame.");
            }

            // No need to .Dispose either of these: their .Dispose and .Flush
            // methods do nothing.
            var memStream = new MemoryStream(TotalSize);
            var binaryWriter = new BinaryWriter(memStream, Encoding.UTF8, leaveOpen: true);

            // Add ensures that we never have more than UInt16 framelets
            var numFramelets = unchecked((UInt16) framelets.Count);
            binaryWriter.Write(numFramelets);

            foreach (var framelet in framelets)
            {
                // Framelet ctor checks that the type is valid, so we don't
                // need to worry about overflow.
                var frameletType = unchecked((UInt16) framelet.Type);
                // ArraySegment checks that the Count is not negative, so we
                // don't need to worry about overflow.
                var frameletLength = unchecked((UInt32) framelet.Contents.Count);

                binaryWriter.Write(frameletType);
                binaryWriter.Write(frameletLength);
                binaryWriter.Write(
                    framelet.Contents.Array,
                    framelet.Contents.Offset,
                    framelet.Contents.Count);
            }

            Debug.Assert(TotalSize == memStream.Position);

            // Since we used the ctor that takes a capacity, we'll be able to
            // get the buffer, and the offset of the buffer will be 0.
            byte[] outputBuffer = memStream.GetBuffer();

            try
            {
                await stream.WriteAsync(outputBuffer, offset: 0, count: TotalSize);
            }
            catch (Exception ex) when (ex is IOException || ex is ObjectDisposedException)
            {
                logger.Site().Error(ex, "Failed to write entire frame.");
                throw;
            }
        }

        public static async Task<Frame> ReadAsync(Stream stream, CancellationToken ct, Logger logger)
        {
            try
            {
                var frameletCount = await ReadUInt16Async(stream, ct);
                if (frameletCount == 0)
                {
                    throw new EpoxyProtocolErrorException("Zero framelets");
                }

                var frame = new Frame(frameletCount, logger);

                while (frameletCount > 0)
                {
                    if (ct.IsCancellationRequested)
                    {
                        return null;
                    }

                    var frameletType = await ReadUInt16Async(stream, ct);
                    if (!Framelet.IsKnownType(frameletType))
                    {
                        throw new EpoxyProtocolErrorException("Unknown framelet type: " + frameletType);
                    }

                    var frameletLength = await ReadUInt32Async(stream, ct);
                    if (frameletLength > int.MaxValue)
                    {
                        throw new EpoxyProtocolErrorException("Framelet too big: " + frameletLength);
                    }

                    byte[] frameletContents = await ReadBufferAsync(stream, unchecked((int)frameletLength), ct);
                    frame.Add(new Framelet((FrameletType)frameletType, new ArraySegment<byte>(frameletContents)));

                    --frameletCount;
                }

                return frame;
            }
            catch (Exception ex) when (ex is OperationCanceledException || ex is EndOfStreamException || ex is ObjectDisposedException)
            {
                return null;
            }
        }

        static async Task<UInt16> ReadUInt16Async(Stream stream, CancellationToken ct)
        {
            var buf = await ReadBufferAsync(stream, sizeof(UInt16), ct);
            return BitConverter.ToUInt16(buf, 0);
        }

        static async Task<UInt32> ReadUInt32Async(Stream stream, CancellationToken ct)
        {
            var buf = await ReadBufferAsync(stream, sizeof(UInt32), ct);
            return BitConverter.ToUInt32(buf, 0);
        }

        static async Task<byte[]> ReadBufferAsync(Stream stream, int length, CancellationToken ct)
        {
            Debug.Assert(stream != null);
            Debug.Assert(length > 0);

            var buf = new byte[length];
            int bytesToRead = length;
            while (bytesToRead > 0)
            {
                ct.ThrowIfCancellationRequested();

                int dataRead = await stream.ReadAsync(buf, buf.Length - bytesToRead, bytesToRead, ct);
                if (dataRead == 0)
                {
                    throw new EndOfStreamException("End of stream encountered while reading fream");
                }

                bytesToRead -= dataRead;
            }

            return buf;
        }
    }
}
