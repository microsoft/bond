// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;

/**
 * Implements Simple JSON protocol reader for object deserialization.
 */
public final class SimpleJsonReader implements TextProtocolReader {

    private final JsonParser parser;

    // indicates whether the parser's actual state is one token ahead of the expected state,
    // which happens when we need to peek ahead for the next token when determining whether
    // an array has elements left (note that methods such as readInt32 can't both return
    // a value and indicate the end of an array, and the API relies on readContainerBegin for
    // the latter purpose)
    private boolean hasPeekedParserForNextToken;

    public SimpleJsonReader(InputStream inputStream) throws IOException {
        ArgumentHelper.ensureNotNull(inputStream, "inputStream");
        this.parser = JsonGlobals.jsonFactory.createParser(inputStream);
        this.hasPeekedParserForNextToken = false;
    }

    @Override
    public final String readStructBegin() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.START_OBJECT) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' is expected.",
                    currentToken,
                    JsonToken.START_OBJECT);
        }
        // this protocol doesn't support struct type names in the payload
        return null;
    }

    @Override
    public final void readStructEnd() throws IOException {
        // nothing to do here, as the END_OBJECT token was already skipped in readFieldBegin
    }

    @Override
    public final String readFieldBegin() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken == JsonToken.FIELD_NAME) {
            // there is a field to read
            return this.parser.getText();
        } else if (currentToken == JsonToken.END_OBJECT) {
            // there are no more fields to read
            return null;
        } else {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where either '%s' or '%s' is expected.",
                    currentToken,
                    JsonToken.FIELD_NAME,
                    JsonToken.END_OBJECT);
        }
    }

    @Override
    public final void readFieldEnd() throws IOException {
        // nothing to do here
    }

    @Override
    public final void readContainerBegin() throws IOException {
        // skip a START_ARRAY token that is expected here
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.START_ARRAY) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' is expected.",
                    currentToken,
                    JsonToken.START_ARRAY);
        }
    }

    @Override
    public final void readContainerEnd() throws IOException {
        // need to clear the peek flag to make sure nested collections work properly
        //
        // note that this amounts to skipping a END_ARRAY token which is expected to
        // be the current (peeked) token when this method is entered
        this.hasPeekedParserForNextToken = false;
    }

    @Override
    public final boolean readContainerItemBegin() throws IOException {
        // peek the next token to see whether it is end of an array
        //
        // note that this is the only place where peekParserForNextToken is called and since clients
        // of TextProtocolReader should call either readXXX (some value) or readContainerEnd between
        // any two calls calls to readContainerItemBegin, the hasPeekedParserForNextToken flag is
        // expected to be clear every time this method is called
        //
        // also note that unless we have reached the end on a container, the caller will invoke
        // one of the methods to read a value, which will call method advanceParserToNextToken,
        // which will check whether the token is null, and thus EOF will be detected when reading
        // a value
        final JsonToken nextToken = this.peekParserForNextToken();
        return nextToken != JsonToken.END_ARRAY;
    }

    @Override
    public final void readContainerItemEnd() throws IOException {
        // nothing to do here
    }

    @Override
    public final byte readInt8() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.VALUE_NUMBER_INT) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' for Bond 'int8' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_NUMBER_INT);
        }
        // an int8 value must be represented by INT (Jackson's minimal "optimal type") and be within bounds
        final JsonParser.NumberType numberType = this.parser.getNumberType();
        if (numberType == JsonParser.NumberType.INT) {
            final int valueAsInt = this.parser.getIntValue();
            if (valueAsInt >= Byte.MIN_VALUE && valueAsInt <= Byte.MAX_VALUE) {
                return (byte) valueAsInt;
            }
        }
        // at this point, we have a NUMBER_INT token with either:
        // (a) number type BIG_INT or LONG, or
        // (b) number type INT but value is outside of int8 bounds
        throw createInvalidBondDataException(
                "Current JSON integer value '%s' is outside of bounds for the expected Bond data type 'int8'.",
                this.parser.getValueAsString());
    }

    @Override
    public final short readInt16() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.VALUE_NUMBER_INT) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' for Bond 'int16' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_NUMBER_INT);
        }
        // an int16 value must be represented by INT (Jackson's minimal "optimal type") and be within bounds
        final JsonParser.NumberType numberType = this.parser.getNumberType();
        if (numberType == JsonParser.NumberType.INT) {
            final int valueAsInt = this.parser.getIntValue();
            if (valueAsInt >= Short.MIN_VALUE && valueAsInt <= Short.MAX_VALUE) {
                return (short) valueAsInt;
            }
        }
        // at this point, we have a NUMBER_INT token with either:
        // (a) number type BIG_INT or LONG, or
        // (b) number type INT but value is outside of int16 bounds
        throw createInvalidBondDataException(
                "Current JSON integer value '%s' is outside of bounds for the expected Bond data type 'int16'.",
                this.parser.getValueAsString());
    }

    @Override
    public final int readInt32() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.VALUE_NUMBER_INT) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' for Bond 'int32' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_NUMBER_INT);
        }
        // an int32 value must be represented by INT (Jackson's minimal "optimal type")
        final JsonParser.NumberType numberType = this.parser.getNumberType();
        if (numberType == JsonParser.NumberType.INT) {
            final int valueAsInt = this.parser.getIntValue();
            return valueAsInt;
        }
        // at this point, we have a NUMBER_INT token with number type BIG_INT or LONG
        throw createInvalidBondDataException(
                "Current JSON integer value '%s' is outside of bounds for the expected Bond data type 'int32'.",
                this.parser.getValueAsString());
    }

    @Override
    public final long readInt64() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.VALUE_NUMBER_INT) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' for Bond 'int64' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_NUMBER_INT);
        }
        // an int64 value must be represented by either LONG or INT (Jackson's "optimal types" for the value)
        final JsonParser.NumberType numberType = this.parser.getNumberType();
        if (numberType == JsonParser.NumberType.LONG || numberType == JsonParser.NumberType.INT) {
            final long valueAsLong = this.parser.getLongValue();
            return valueAsLong;
        }
        // at this point, we have a NUMBER_INT token with number type BIG_INT
        throw createInvalidBondDataException(
                "Current JSON integer value '%s' is outside of bounds for the expected Bond data type 'int64'.",
                this.parser.getValueAsString());
    }

    @Override
    public final byte readUInt8() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.VALUE_NUMBER_INT) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' for Bond 'uint8' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_NUMBER_INT);
        }
        // an uint8 value must be represented by INT (Jackson's minimal "optimal type") and be within bounds
        final JsonParser.NumberType numberType = this.parser.getNumberType();
        if (numberType == JsonParser.NumberType.INT) {
            final int valueAsInt = this.parser.getIntValue();
            if (valueAsInt >= 0 && valueAsInt <= UnsignedHelper.MAX_UINT8_VALUE) {
                return (byte) valueAsInt;
            }
        }
        // at this point, we have a NUMBER_INT token with either:
        // (a) number type BIG_INT or LONG, or
        // (b) number type INT but value is outside of uint8 bounds
        throw createInvalidBondDataException(
                "Current JSON integer value '%s' is outside of bounds for the expected Bond data type 'uint8'.",
                this.parser.getValueAsString());
    }

    @Override
    public final short readUInt16() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.VALUE_NUMBER_INT) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' for Bond 'uint16' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_NUMBER_INT);
        }
        // an uint16 value must be represented by INT (Jackson's minimal "optimal type") and be within bounds
        final JsonParser.NumberType numberType = this.parser.getNumberType();
        if (numberType == JsonParser.NumberType.INT) {
            final int valueAsInt = this.parser.getIntValue();
            if (valueAsInt >= 0 && valueAsInt <= UnsignedHelper.MAX_UINT16_VALUE) {
                return (short) valueAsInt;
            }
        }
        // at this point, we have a NUMBER_INT token with either:
        // (a) number type BIG_INT or LONG, or
        // (b) number type INT but value is outside of uint16 bounds
        throw createInvalidBondDataException(
                "Current JSON integer value '%s' is outside of bounds for the expected Bond data type 'uint16'.",
                this.parser.getValueAsString());
    }

    @Override
    public final int readUInt32() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.VALUE_NUMBER_INT) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' for Bond 'uint32' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_NUMBER_INT);
        }
        // an uint32 value must be represented by either INT or LONG and be within respectable bounds
        final JsonParser.NumberType numberType = this.parser.getNumberType();
        if (numberType == JsonParser.NumberType.INT) {
            final int valueAsInt = this.parser.getIntValue();
            if (valueAsInt >= 0) {
                return valueAsInt;
            }
        } else if (numberType == JsonParser.NumberType.LONG) {
            final long valueAsLong = this.parser.getLongValue();
            if (valueAsLong >= 0 && valueAsLong <= UnsignedHelper.MAX_UINT32_VALUE) {
                return (int) valueAsLong;
            }
        }
        // at this point, we have a NUMBER_INT token with either:
        // (a) number type BIG_INT, or
        // (b) number type INT but value is negative, or
        // (c) number type LONG but value is outside of uint32 bounds
        throw createInvalidBondDataException(
                "Current JSON integer value '%s' is outside of bounds for the expected Bond data type 'uint32'.",
                this.parser.getValueAsString());
    }

    @Override
    public final long readUInt64() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.VALUE_NUMBER_INT) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' for Bond 'uint64' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_NUMBER_INT);
        }
        // an uint64 value must be represented by either INT, LONG or BIG_INT and be within respectable bounds
        final JsonParser.NumberType numberType = this.parser.getNumberType();
        if (numberType == JsonParser.NumberType.INT) {
            final int valueAsInt = this.parser.getIntValue();
            if (valueAsInt >= 0) {
                return (int) valueAsInt;
            }
        } else if (numberType == JsonParser.NumberType.LONG) {
            final long valueAsLong = this.parser.getLongValue();
            if (valueAsLong >= 0) {
                return valueAsLong;
            }
        } else if (numberType == JsonParser.NumberType.BIG_INTEGER) {
            final BigInteger valueAsBigInteger = this.parser.getBigIntegerValue();
            if (valueAsBigInteger.signum() >= 0 && valueAsBigInteger.compareTo(UnsignedHelper.MAX_UINT64_VALUE) <= 0) {
                // gets only the lower 64 bits
                return valueAsBigInteger.longValue();
            }
        }
        // at this point, we have a NUMBER_INT token with either:
        // (a) number type INT but value is negative, or
        // (b) number type LONG but value is negative, or
        // (c) number type BIG_INT but value is outside of uint64 bounds
        throw createInvalidBondDataException(
                "Current JSON integer value '%s' is outside of bounds for the expected Bond data type 'uint64'.",
                this.parser.getValueAsString());
    }

    @Override
    public final float readFloat() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.VALUE_NUMBER_FLOAT && currentToken != JsonToken.VALUE_NUMBER_INT) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where either '%s' or '%s' for Bond 'float' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_NUMBER_FLOAT,
                    JsonToken.VALUE_NUMBER_INT);
        }
        // rely on Jackson to parse the current token as a float
        try {
            final float valueAsFloat = this.parser.getFloatValue();
            return valueAsFloat;
        } catch (JsonParseException e) {
            throw createInvalidBondDataException(
                    e,
                    "Current JSON float value '%s' is outside of bounds for the expected Bond data type 'float'.",
                    this.parser.getValueAsString());
        }
    }

    @Override
    public final double readDouble() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken != JsonToken.VALUE_NUMBER_FLOAT && currentToken != JsonToken.VALUE_NUMBER_INT) {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where either '%s' or '%s' for Bond 'double' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_NUMBER_FLOAT,
                    JsonToken.VALUE_NUMBER_INT);
        }
        // rely on Jackson to parse the current token as a double
        try {
            final double valueAsDouble = this.parser.getDoubleValue();
            return valueAsDouble;
        } catch (JsonParseException e) {
            throw createInvalidBondDataException(
                    e,
                    "Current JSON float value '%s' is outside of bounds for the expected Bond data type 'double'.",
                    this.parser.getValueAsString());
        }
    }

    @Override
    public final boolean readBool() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken == JsonToken.VALUE_FALSE) {
            return false;
        } else if (currentToken == JsonToken.VALUE_TRUE) {
            return true;
        } else {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where either '%s' or '%s' for Bond 'bool' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_FALSE,
                    JsonToken.VALUE_TRUE);
        }
    }

    @Override
    public final String readString() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken == JsonToken.VALUE_STRING) {
            final String valueAsString = this.parser.getValueAsString();
            return valueAsString;
        } else {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' for Bond 'string' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_STRING);
        }
    }

    @Override
    public final String readWString() throws IOException {
        final JsonToken currentToken = this.advanceParserToNextToken();
        if (currentToken == JsonToken.VALUE_STRING) {
            final String valueAsString = this.parser.getValueAsString();
            return valueAsString;
        } else {
            throw createInvalidBondDataException(
                    "Current JSON token is '%s' where '%s' for Bond 'wstring' data type is expected.",
                    currentToken,
                    JsonToken.VALUE_STRING);
        }
    }

    @Override
    public void skip(BondDataType bondDataType) throws IOException {
        // no support for skipping (which is required for Bonded functionality)
        throw new UnsupportedOperationException("Skipping is not implemented.");
    }

    @Override
    public final SimpleJsonReader cloneProtocolReader() throws IOException {
        // no support for cloning (which is required for Bonded functionality)
        throw new UnsupportedOperationException("Cloning is not implemented.");
    }

    /**
     * Advances the JSON parser to the next token and checks whether the end of input has been reached
     * (in which case the method throws an exception). Returns a non-null token.
     */
    private JsonToken advanceParserToNextToken() throws IOException {
        final JsonToken currentToken;
        if (this.hasPeekedParserForNextToken) {
            // if has already peeked the next token (i.e. peekParserForNextToken was called),
            // then consume the current token without advancing the parser, which will
            // be advanced the next time this method (or peekParserForNextToken) is called
            currentToken = this.parser.currentToken();
            this.hasPeekedParserForNextToken = false;
        } else {
            // otherwise, advance to the next token
            try {
                currentToken = this.parser.nextToken();
            } catch (JsonProcessingException e) {
                // wrap Jackson exception by a Bond exception
                throw createInvalidBondDataException(e, "Error processing JSON input: " + e.getMessage());
            }
        }
        // check the current token for the end of stream
        if (currentToken == null) {
            throw new EOFException("The end of JSON input has been reached.");
        }
        return currentToken;
    }

    /**
     * Peeks the next token within the JSON parser and returns it (possibly null).
     * Can peek a token only once; subsequent calls will return the same token
     * until {@link #advanceParserToNextToken} is called to advance the parser.
     */
    private JsonToken peekParserForNextToken() throws IOException {
        final JsonToken nextToken;
        if (this.hasPeekedParserForNextToken) {
            // if has already peeked once, consume the current token
            nextToken = this.parser.currentToken();
        } else {
            // otherwise, advance the parser and get the next token
            // which may be null (when the end of the input is reached)
            try {
                nextToken = this.parser.nextToken();
            } catch (JsonProcessingException e) {
                // wrap Jackson exception by a Bond exception
                throw createInvalidBondDataException(e, "Error processing JSON input: " + e.getMessage());
            }
            this.hasPeekedParserForNextToken = true;
        }
        return nextToken;
    }

    /**
     * Creates a new {@link InvalidBondDataException} exception which indicates a Bond-specific protocol error.
     */
    private static InvalidBondDataException createInvalidBondDataException(
            String messageFormat, Object... messageArgs) {
        return createInvalidBondDataException(null, messageFormat, messageArgs);
    }

    /**
     * Creates a new {@link InvalidBondDataException} exception which indicates a Bond-specific protocol error.
     */
    private static InvalidBondDataException createInvalidBondDataException(
            Exception causeException, String messageFormat, Object... messageArgs) {
        String message = String.format(messageFormat, messageArgs);
        return new InvalidBondDataException(message, causeException);
    }
}
