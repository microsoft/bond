// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Indicates that Bond data is in invalid format.
 */
public class InvalidBondDataException extends IOException {

    public InvalidBondDataException(String message) {
        super(message);
    }

    public InvalidBondDataException(String message, Throwable cause) {
        super(message, cause);
    }

    public InvalidBondDataException(Throwable cause) {
        super(cause);
    }
}
