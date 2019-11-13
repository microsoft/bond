// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.IO.Unsafe
{
    using System;
    using System.ComponentModel;
    using System.IO;
    using System.Linq.Expressions;
    using System.Reflection;
    using System.Runtime.InteropServices;
    using System.Security;
    using Microsoft.Win32.SafeHandles;

    internal static class StreamCloning
    {
        static readonly FileShare[] shareModes =
        {
            FileShare.Read, FileShare.ReadWrite, FileShare.ReadWrite | FileShare.Delete
        };

        public static T Clone<T>(this T stream) where T : Stream, ICloneable<T>
        {
            return stream.Clone();
        }

        public static Stream Clone(this Stream stream)
        {
            var memoryStream = stream as MemoryStream;
            if (memoryStream != null)
            {
                return Clone(memoryStream);
            }

            var fileStream = stream as FileStream;
            if (fileStream != null)
            {
                return Clone(fileStream);
            }

            throw new NotSupportedException("Stream type " + stream.GetType().FullName + " can't be cloned.");
        }
        
        static MemoryStream Clone(MemoryStream stream)
        {
            return MemoryStreamCloner.CloneMemoryStream(stream);
        }
        
        static FileStream Clone(FileStream stream)
        {
            var error = 0;
            foreach (var mode in shareModes)
            {
                var handle = NativeMethods.ReOpenFile(stream.SafeFileHandle, NativeMethods.FILE_GENERIC_READ, mode, 0);
                error = Marshal.GetLastWin32Error();

                if (error == NativeMethods.NO_ERROR)
                {
                    return new FileStream(handle, FileAccess.Read) { Position = stream.Position };
                }

                if (error != NativeMethods.ERROR_SHARING_VIOLATION)
                {
                    break;
                }
            }

            throw new Win32Exception(error);
        }
        
        static class NativeMethods
        {
            public const int NO_ERROR = 0;
            public const int ERROR_SHARING_VIOLATION = 32;
            public const uint FILE_GENERIC_READ = 0x00120089;

            [DllImport("kernel32.dll", SetLastError = true)]
#if !(NETSTANDARD1_3 || NETSTANDARD1_6)
            [SuppressUnmanagedCodeSecurity]
#endif
            public static extern SafeFileHandle ReOpenFile(SafeFileHandle hFile, uint access, FileShare mode, uint flags);
        }

        static class MemoryStreamCloner
        {
#if !(NET46 || NETSTANDARD1_3 || NETSTANDARD1_6)
            delegate void GetOriginAndLength(MemoryStream stream, out int origin, out int length);
            static readonly GetOriginAndLength getOriginAndLength;

            static MemoryStreamCloner()
            {
                // MemoryStream can be created from byte[] at non-zero origin offset which
                // is not exposed via public APIs.
                // TODO: handle the case when the method API is not available (e.g. copy buffer)
                var internalGetOriginAndLength = typeof(MemoryStream).GetMethod(
                    "InternalGetOriginAndLength", BindingFlags.NonPublic | BindingFlags.Instance);

                var stream = Expression.Parameter(typeof(MemoryStream));
                var origin = Expression.Parameter(typeof(int).MakeByRefType());
                var length = Expression.Parameter(typeof(int).MakeByRefType());

                //internal method InternalGetOriginAndLength exists in .NET
                if (internalGetOriginAndLength != null)
                {
                    getOriginAndLength = Expression.Lambda<GetOriginAndLength>(
                        Expression.Call(stream, internalGetOriginAndLength, origin, length), stream, origin, length)
                        .Compile();
                }
                else
                {
                    //Mono uses 'initialIndex' and 'length' fields in memory stream to handle the data
                    //see ref: https://github.com/mono/mono/blob/989301647e7cdf93acb69aacabcbb2bb2884e041/mcs/class/corlib/System.IO/MemoryStream.cs#L122-L126
                    var assign1 = Expression.Assign(origin, Expression.Field(stream, "initialIndex")); //origin = stream.initialIndex
                    var assign2 = Expression.Assign(length, Expression.Field(stream, "length")); //length = stream.length

                    getOriginAndLength = Expression.Lambda<GetOriginAndLength>(
                        Expression.Block(assign1, assign2), stream, origin, length)
                        .Compile();
                }
            }

            internal static MemoryStream CloneMemoryStream(MemoryStream stream)
            {
                int origin, length;
                getOriginAndLength(stream, out origin, out length);
                return new MemoryStream(stream.GetBuffer(), origin, length - origin, false, true) { Position = stream.Position };
            }
#else // NET46 & NETSTANDARD implementation
            internal static MemoryStream CloneMemoryStream(MemoryStream stream)
            {
                ArraySegment<byte> buffer;
                if (!stream.TryGetBuffer(out buffer))
                {
                    // we have to make a copy
                    buffer = new ArraySegment<byte>(stream.ToArray());
                }

                return new MemoryStream(buffer.Array, buffer.Offset, buffer.Count, false, true) { Position = stream.Position };
            }
#endif
        }
    }
}
