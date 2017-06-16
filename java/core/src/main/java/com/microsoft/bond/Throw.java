// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

import java.util.Locale;

/**
 * Encapsulates Bond exception throwing and error messages.
 * Some message details (e.g. list indices, map keys) may be added to
 * exception message by certain specialized implementations of Bond type.
 */
final class Throw {

    // invariant locale
    private static final Locale LOCALE = Locale.ROOT;

    // general entry point to raise a data exception on serialization or deserialization
    static void raiseInvalidDataError(
            Exception cause,
            String format,
            Object... args) throws InvalidBondDataException {
        String message = String.format(LOCALE, format, args);
        throw new InvalidBondDataException(message, cause);
    }

    // general entry point to raise a data exception on value comparison
    static void raiseIllegalArgumentException(
            Exception cause,
            String format,
            Object... args) throws InvalidBondDataException {
        String message = String.format(LOCALE, format, args);
        throw new IllegalArgumentException(message, cause);
    }

    // raises a data exception on (se)serialization of a struct field with details to identify the field;
    // this exception should be the at the top of exception chains caused by Bond data errors when (de)serializing
    static void raiseStructFieldSerializationError(
            boolean isDeserialization,
            StructBondType.StructField<?> field,
            InvalidBondDataException cause,
            String additionalMessageFormat,
            Object... additionalMessageArgs) throws InvalidBondDataException {
        raiseInvalidDataError(
                cause,
                "Unable to %sserialize '%s' field '%s' (id: %d) of struct '%s'%s.",
                (isDeserialization ? "de" : ""),
                field.getFieldType().getFullName(),
                field.getName(),
                field.getId(),
                field.getStructType().getFullName(),
                buildMessageSuffix(cause, additionalMessageFormat, additionalMessageArgs));
    }

    // raises a data exception on (de)serialization of a list/set element with details to identify the element
    static void raiseListContainerElementSerializationError(
            boolean isDeserialization,
            boolean isSet,
            String typeName,
            int index,
            InvalidBondDataException cause,
            String additionalMessageFormat,
            Object... additionalMessageArgs) throws InvalidBondDataException {
        String messageSuffix;
        raiseInvalidDataError(
                cause,
                "Unable to %sserialize element at position %d of %s container '%s'%s.",
                (isDeserialization ? "de" : ""),
                index,
                (isSet ? "Set" : "List"),
                typeName,
                buildMessageSuffix(cause, additionalMessageFormat, additionalMessageArgs));
    }

    // raises a data exception on (de)serialization of a map entry with details to identify the entry
    static void raiseMapContainerElementSerializationError(
            boolean isDeserialization,
            String typeName,
            int index,
            Object keyValue,
            InvalidBondDataException cause,
            String additionalMessageFormat,
            Object... additionalMessageArgs) throws InvalidBondDataException {
        String messageSuffix;
        raiseInvalidDataError(
                cause,
                "Unable to %sserialize %s at position %d%s of Map container '%s'%s.",
                (isDeserialization ? "de" : ""),
                (keyValue == null ? "value" : "key"),
                index,
                (keyValue == null ? "" : (" (key=" + keyValue + ")")),
                typeName,
                buildMessageSuffix(cause, additionalMessageFormat, additionalMessageArgs));
    }

    private static String buildMessageSuffix(
            Throwable cause,
            String additionalMessageFormat,
            Object[] additionalMessageArgs) {
        if (additionalMessageFormat != null) {
            return ", " + String.format(LOCALE, additionalMessageFormat, additionalMessageArgs);
        } else if (cause != null) {
            return ", refer to the cause exception for more details.";
        } else {
            return "";
        }
    }

    // raised when encountering a non-nullable value of a field or a collection element that is set to null,
    // this exception should be chained to another exception that indicates the field or the collection element
    static void raiseNonNullableValueSetTuNullError(
            String typeName) throws InvalidBondDataException {
        raiseInvalidDataError(
                null,
                "Unable to serialize a non-nullable '%s' value that is set to null.",
                typeName);
    }

    // raised when deserializing a value of type that is not compatible with the type of the field
    static void raiseFieldTypeIsNotCompatibleDeserializationError(
            BondDataType deserializedBondDataType,
            StructBondType.StructField<?> field) throws InvalidBondDataException {
        raiseStructFieldSerializationError(
                true,
                field,
                null,
                "payload type '%s' is not compatible with the field type '%s'",
                deserializedBondDataType,
                field.getFieldType().getBondDataType());
    }

    // raised when deserializing a nullable value that has more than one element in the list representation
    static void raiseNullableListValueHasMultipleElementsDeserializationError(
            String typeName) throws InvalidBondDataException {
        raiseInvalidDataError(
                null,
                "Unable to deserialize '%s' since the payload contains more than one element.",
                typeName);
    }

    // raised when deserializing a container whose element (or key/value) type is not compatible with the expected type
    static void raiseContainerElementTypeIsNotCompatibleDeserializationError(
            String entityName,
            BondDataType deserializedBondDataType,
            BondDataType declaredBondDataType,
            String typeName) throws InvalidBondDataException {
        raiseInvalidDataError(
                null,
                "Unable to deserialize '%s' since payload %s type '%s' is not compatible with declared %s type '%s'.",
                typeName,
                entityName,
                deserializedBondDataType,
                entityName,
                declaredBondDataType);
    }

    // raised when deserializing a struct that is missing a required field
    static void raiseRequiredStructFieldIsMissingDeserializationError(
            StructBondType.StructField<?> field) throws InvalidBondDataException {
        raiseInvalidDataError(
                null,
                "Unable to deserialize '%s' since the payload is missing required field '%s' (id: %d) of type '%s'.",
                field.getStructType().getFullName(),
                field.getName(),
                field.getId(),
                field.getFieldType().getFullName());
    }

    // raised when deserializing a struct field that is present in the payload more than once
    static void raiseStructFieldIsPresentMoreThanOnceDeserializationError(
            StructBondType.StructField<?> field) throws InvalidBondDataException {
        raiseInvalidDataError(
                null,
                "Unable to deserialize '%s' since the payload contains multiple fields '%s' (id: %d) of type '%s'.",
                field.getStructType().getFullName(),
                field.getName(),
                field.getId(),
                field.getFieldType().getFullName());
    }
}
