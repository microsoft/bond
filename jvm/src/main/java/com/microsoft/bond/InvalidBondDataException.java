package com.microsoft.bond;

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
