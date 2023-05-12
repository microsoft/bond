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
            public static extern SafeFileHandle ReOpenFile(SafeFileHandle hFile, uint access, FileShare mode, uint flags);
        }

        static class MemoryStreamCloner
        {
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
        }
    }
}
