package org.bondlib;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;

/**
 * Contains shared global references and constants.
 */
final class JsonGlobals {

    // thread-safe shared factory for parsers
    static final JsonFactory jsonFactory = new JsonFactory();

    static {
        jsonFactory.enable(JsonParser.Feature.ALLOW_NON_NUMERIC_NUMBERS);
    }
}
