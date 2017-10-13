// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;

/**
 * Contains shared global references and constants.
 */
final class JsonGlobals {

    // prevent instantiation
    private JsonGlobals() {
    }

    /**
     * Thread-safe shared factory for parsers (see {@link JsonFactory} javadoc for details.
     */
    static final JsonFactory jsonFactory = new JsonFactory();

    static {
        // Allow parsing and generating non-numeric floating-point values: (+/-)Infinity and NaN,
        // to be able to properly roundtrip (i.e. be able to parse output that we generate) when
        // a float or double field is set to such value.
        jsonFactory.enable(JsonParser.Feature.ALLOW_NON_NUMERIC_NUMBERS);
    }
}
