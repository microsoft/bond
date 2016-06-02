// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Examples.Logging
{
    using System;

    using Bond.Comm;

    public class ConsoleLogger : LogHandler
    {
        public void Handle(LogSeverity severity, Exception exception, String format, params object[] args)
        {
            if (severity < LogSeverity.Information) { return; }

            var message = string.Format(format, args);
            Console.WriteLine($"[bond] {severity.ToString().ToUpper()}: {message}");
            if (exception != null)
            {
                Console.WriteLine(exception);
            }
        }
    }
}
