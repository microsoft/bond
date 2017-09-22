// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Indicates that a Bond protocol is not supported within the current context.
 */
public class UnsupportedProtocolException extends IOException {

    public UnsupportedProtocolException(String message) {
        super(message);
    }

    public UnsupportedProtocolException(String message, Throwable cause) {
        super(message, cause);
    }

    public UnsupportedProtocolException(Throwable cause) {
        super(cause);
    }
}
