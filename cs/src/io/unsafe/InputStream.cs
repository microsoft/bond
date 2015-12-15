// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.IO.Unsafe
{
    using System;
    using System.IO;

    /// <summary>
    /// Implements IInputStream on top of System.Stream
    /// </summary>
    public class InputStream : InputBuffer, ICloneable<InputStream>
    {
        readonly Stream stream;
        readonly int bufferLength;
        // When we read more data from the stream we can override existing buffer
        // only if it hasn't been exposed via ReadBytes or Clone. Otherwise a new
        // buffer has to be allocated.
        bool canReuseBuffer;

        public override long Length
        {
            get { return stream.Length; }
        }

        public override long Position
        {
            get { return stream.Position - (end - position); }
            set { position = checked ((int)(value - stream.Position)) + end; }
        }

        public InputStream(Stream stream, int bufferLength = 64 * 1024)
            : base(new byte[bufferLength], 0, 0)
        {
            this.stream = stream;
            this.bufferLength = bufferLength;
            canReuseBuffer = true;
        }

        InputStream(InputStream that)
            : base(that)
        {
            stream = that.stream.Clone();
            bufferLength = that.bufferLength;
            canReuseBuffer = false;
        }

        /// <summary>
        /// Create a clone of the current state of the buffer
        /// </summary>
        public new InputStream Clone()
        {
            canReuseBuffer = false;
            return new InputStream(this);
        }

        /// <summary>
        /// Read an array of bytes verbatim
        /// </summary>
        /// <param name="count">Number of bytes to read</param>
        /// <exception cref="EndOfStreamException"/>
        public override ArraySegment<byte> ReadBytes(int count)
        {
            var result = base.ReadBytes(count);
            canReuseBuffer = false;
            return result;
        }
        
        internal override void EndOfStream(int count)
        {
            var oldBuffer = buffer;

            if (!canReuseBuffer || count > buffer.Length)
            {
                buffer = new byte[Math.Max(bufferLength, count)];
                canReuseBuffer = true;
            }

            var remaining = end - position;

            if (remaining > 0)
            {
                Buffer.BlockCopy(oldBuffer, position, buffer, 0, remaining);
            }
            else if (remaining < 0)
            {
                stream.Seek(-remaining, SeekOrigin.Current);
                remaining = 0;
            }

            end = remaining + stream.Read(buffer, remaining, buffer.Length - remaining);
            position = 0;

            if (count > end)
            {
                base.EndOfStream(count - end);
            }
        }
    }
}
