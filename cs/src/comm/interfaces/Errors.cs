// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Collections.Generic;

    public static class Errors
    {
        /// <summary>
        /// (Opaque) error message returned to client for server-side issues.
        /// </summary>
        public const string InternalErrorMessage = "The server has encounted an error";

        /// <summary>
        /// Checks if an <see cref="Error" /> is an <see cref="InternalServerError"/>.
        /// If it is, strips out all information except the <c>error_code</c> and
        /// <c>unique_id</c> and sets the message to <see cref="InternalErrorMessage"/>.
        /// </summary>
        /// <param name="error">The error</param>
        /// <returns>The error that was passed in.</returns>
        public static Error CleanseInternalServerError(Error error)
        {
            var internalError = error as InternalServerError;

            if (internalError != null)
            {
                internalError.message = InternalErrorMessage;
                internalError.inner_error = null;
                internalError.server_stack_trace = string.Empty;
            }
            return error;
        }

        /// <summary>
        /// Creates an <see cref="InternalServerError"/> with the given message.
        /// </summary>
        /// <param name="message">An error message.</param>
        /// <param name="uniqueId">The ID of the request that caused this error. This should not be null,
        /// but if it is, the generated error will contain an empty string.</param>
        /// <returns>An InternalServerError representing the exception.</returns>
        public static InternalServerError MakeInternalServerError(string message, string uniqueId)
        {
            var internalServerError = new InternalServerError
            {
                error_code = (int) ErrorCode.INTERNAL_SERVER_ERROR,
                unique_id = uniqueId ?? string.Empty,
                message = message ?? InternalErrorMessage
            };


            return internalServerError;
        }

        /// <summary>
        /// Creates an <see cref="InternalServerError"/> from an exception.
        /// </summary>
        /// <param name="exception">An exception.</param>
        /// <param name="uniqueId">The ID of the request that caused this error. This should not be null,
        /// but if it is, the generated error will contain an empty string.</param>
        /// <param name="includeDetails">
        /// <c>true</c> if debugging details should be included; <c>false</c>
        /// to omit this potentailly sensitive information
        /// </param>
        /// <returns>An InternalServerError representing the exception.</returns>
        public static InternalServerError MakeInternalServerError(Exception exception, string uniqueId, bool includeDetails)
        {
            var internalServerError = new InternalServerError
            {
                error_code = (int)ErrorCode.INTERNAL_SERVER_ERROR,
                unique_id = uniqueId ?? string.Empty
            };

            if (includeDetails && exception != null)
            {
                internalServerError.message = InternalErrorMessage + ": " + exception.Message;
                internalServerError.server_stack_trace = exception.StackTrace;

                var aggEx = exception as AggregateException;
                if (aggEx != null)
                {
                    var aggregateError = new AggregateError
                    {
                        error_code = (int) ErrorCode.MULTIPLE_ERRORS_OCCURRED,
                        message = "One or more errors occured",
                        inner_errors = new List<IBonded<Error>>(aggEx.InnerExceptions.Count)
                    };

                    foreach (var innerException in aggEx.InnerExceptions)
                    {
                        var innerError = MakeInternalServerError(innerException, uniqueId, includeDetails);
                        aggregateError.inner_errors.Add(new Bonded<InternalServerError>(innerError));
                    }

                    internalServerError.inner_error = new Bonded<AggregateError>(aggregateError);
                }
                else if (exception.InnerException != null)
                {
                    var innerError = MakeInternalServerError(exception.InnerException, uniqueId, includeDetails);
                    internalServerError.inner_error = new Bonded<InternalServerError>(innerError);
                }
            }
            else
            {
                internalServerError.message = InternalErrorMessage;
            }

            return internalServerError;
        }
    }
}
