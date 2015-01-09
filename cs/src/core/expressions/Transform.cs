// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System;
    using System.Collections.Generic;
    using System.Linq.Expressions;

    internal delegate Expression BeginHandler();
    internal delegate Expression EndHandler();
    internal delegate Expression BaseHandler(IParser parser);
    internal delegate Expression UnknownEndHandler();
    internal delegate Expression KnownFieldHandler(IParser parser, Expression fieldType);
    internal delegate Expression UnknownFieldHandler(IParser parser, Expression fieldType, Expression fieldId);
    internal delegate Expression OmittedFieldHandler();

    internal class Field : IField
    {
        readonly ushort id;
        readonly KnownFieldHandler value;
        readonly OmittedFieldHandler omitted;

        public Field(ushort Id, KnownFieldHandler Value, OmittedFieldHandler Omitted = null)
        {
            id = Id;
            value = Value;
            omitted = Omitted;
        }

        UInt16 IField.Id
        {
            get { return id; }
        }

        Expression IField.Value(IParser parser, Expression valueType)
        {
            return value(parser, valueType);
        }

        Expression IField.Omitted
        {
            get { return omitted != null ? omitted() : Expression.Empty(); }
        }
    }

    internal class Transform : ITransform
    {
        readonly BeginHandler begin;
        readonly EndHandler end;
        readonly BaseHandler base_;
        readonly UnknownEndHandler unknownEnd;
        readonly IEnumerable<IField> fields;
        readonly UnknownFieldHandler unknownField;

        public Transform(
            BeginHandler Begin = null,
            EndHandler End = null,
            IEnumerable<IField> Fields = null,
            UnknownFieldHandler UnknownField = null,
            BaseHandler Base = null,
            UnknownEndHandler UnknownEnd = null)
        {
            begin = Begin;
            end = End;
            base_ = Base;
            unknownEnd = UnknownEnd;
            fields = Fields ?? new IField[0];
            unknownField = UnknownField;
        }

        Expression ITransform.Begin
        {
            get { return begin != null ? begin() : Expression.Empty(); }
        }

        Expression ITransform.End
        {
            get { return end != null ? end() : Expression.Empty(); }
        }

        Expression ITransform.Base(IParser parser)
        {
            return base_ != null ? base_(parser) : Expression.Empty();
        }

        Expression ITransform.UnknownEnd
        {
            get { return unknownEnd != null ? unknownEnd() : Expression.Empty(); }
        }

        IEnumerable<IField> ITransform.Fields
        {
            get { return fields; }
        }

        Expression ITransform.UnknownField(IParser parser, Expression fieldType, Expression fieldId)
        {
            return unknownField != null ? unknownField(parser, fieldType, fieldId) : null;
        }
    }
}
