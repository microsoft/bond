// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Indicates that a stream cannot be cloned because the original stream is not cloneable.
 */
public class StreamNotCloneableException extends IOException {

    public StreamNotCloneableException(String message) {
        super(message);
    }

    public StreamNotCloneableException(String message, Throwable cause) {
        super(message, cause);
    }

    public StreamNotCloneableException(Throwable cause) {
        super(cause);
    }
}
