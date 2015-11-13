// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions.Pull
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Linq;
    using System.Linq.Expressions;
    
    using Bond.Expressions;
    
    public abstract class PullParser<T> : IParser
    {
        readonly RuntimeSchema schema;

        /// <summary>
        /// Indicates whether fields parsing is flattened. When true, the generated state
        /// machine expression will include detection and handling of fields defined in
        /// the base schemas.
        /// </summary>
        readonly bool flatten;
        
        protected PullParser(RuntimeSchema schema, bool flatten)
        {
            Audit.ArgRule(schema.HasValue, "PullParser requires runtime schema");
            this.schema = schema;
            this.flatten = flatten;
        }

        protected PullParser(PullParser<T> that, RuntimeSchema schema, bool flatten)
        {
            Audit.ArgNotNull(that, "that");
            Audit.ArgRule(schema.HasValue, "PullParser requires runtime schema");
            this.schema = schema;
            this.flatten = flatten;
        }

        protected RuntimeSchema Schema { get { return schema; } }

        #region PullParser Abstracts
        /// <summary>
        /// Returns an expression that pulls the next step off the reader.
        /// </summary>
        /// <returns></returns>
        protected abstract Expression Read();

        /// <summary>
        /// Returns an expression that indicates if the end of the input has been reached.
        /// </summary>
        protected abstract Expression EOF { get; }

        /// <summary>
        /// Returns an expression that indicates the type of the currently pulled step.
        /// </summary>
        /// <returns></returns>
        protected abstract Expression Token();
        
        #endregion

        #region IParser Implementation
        public virtual Expression Apply(ITransform transform)
        {
            IEnumerable<TransformSchemaPair> transforms;

            if (flatten && schema.HasBase)
            {
                // collect transform/schema pairs for all the bases
                var flatteningParser = new FlatteningParser(Schema);
                flatteningParser.Apply(transform);
                transforms = flatteningParser.Transforms;
            }
            else
            {
                transforms = new[] { new TransformSchemaPair(transform, Schema) };
            }

            var state = Expression.Variable(typeof(byte), "state");
            var requiredFields = RequiredFields.Variable("requiredFields");
            var requiredFieldNames = GetRequiredFieldNames();

            var stateMachine = CreateStateMachine(transforms, requiredFields);

            return Expression.Block(
                new[] { state, requiredFields },
                transform.Begin,
                
                RequiredFields.Init(requiredFields, requiredFieldNames.Count),

                Expression.Assign(state, Expression.Constant(stateMachine.InitialState)),
                
                ControlExpression.While(
                    Expression.AndAlso(
                        Expression.Not(EOF),
                        Expression.NotEqual(state, Expression.Constant(stateMachine.FinalState))),
                    SwitchToken(stateMachine, state),
                    Expression.Label("endStateMachineLoop")),

                VerifyRequiredFields(requiredFields, requiredFieldNames),
                
                transform.End);
        }

        IList<string> GetRequiredFieldNames()
        {
            var requiredFieldNames = new List<string>();
            for (var s = Schema; s.HasValue; s = flatten ? s.GetBaseSchema() : RuntimeSchema.Empty)
            {
                var currentSchema = s;
                requiredFieldNames.AddRange(currentSchema.StructDef.fields
                    .Where(f => f.metadata.modifier == Modifier.Required)
                    .Select(f => currentSchema.StructDef.metadata.qualified_name + "." + f.metadata.name));
            }

            return requiredFieldNames;
        }

        static Expression VerifyRequiredFields(ParameterExpression fields, IList<string> names)
        {
            if (names.Count == 0)
            {
                return Expression.Empty();
            }

            return RequiredFields.IfMissingAny(
                fields,
                ThrowExpression.RequiredFieldsMissingException(fields, names));
        }
        
        public abstract Expression Container(BondDataType? expectedType, ContainerHandler handler);

        public abstract Expression Map(BondDataType? expectedKeyType, BondDataType? expectedValueType, MapHandler handler);

        public abstract Expression Blob(Expression count);

        public abstract Expression Scalar(Expression valueType, BondDataType expectedType, ValueHandler handler);

        public virtual Expression Bonded(ValueHandler handler)
        {
            throw new NotImplementedException();
        }

        public virtual Expression Skip(Expression valueType)
        {
            throw new NotImplementedException();
        }

        public abstract ParameterExpression ReaderParam { get; }
        
        public abstract Expression ReaderValue { get; }
        
        public int HierarchyDepth { get { return 0; } }
        
        public bool IsBonded { get { return false; } }

        #endregion

        public override bool Equals(object that)
        {
            Debug.Assert(that is PullParser<T>);
            return Comparer.Equal(schema.TypeDef, (that as PullParser<T>).schema.TypeDef);
        }

        public override int GetHashCode()
        {
            return schema.TypeDef.CalculateHashCode();
        }

        #region State machine creation

        protected abstract IStateMachine<T> CreateStateMachine(
            IEnumerable<TransformSchemaPair> transforms, 
            ParameterExpression requiredFields);

        #endregion

        #region State machine codeGen

        Expression SwitchToken(IStateMachine<T> machine, Expression state)
        {
            // add switch cases for each token
            var tokenCases = machine.TokenTransitions.Select(transition => CaseForToken(transition, state));

            // if any ignored tokens are defined, add a switch case for them
            if (machine.IgnoredTokens != null && machine.IgnoredTokens.Any())
            {
                tokenCases = tokenCases.Concat(
                    new[]
                        {
                            Expression.SwitchCase(
                                Expression.Block(
                                    Read(), 
                                    Expression.Empty()), 
                                machine.IgnoredTokens.Select(t => Expression.Constant(t)))
                        });
            }

            return Expression.Switch(
                Token(),
                machine.Default(state),
                tokenCases.ToArray());
        }
        
        static SwitchCase CaseForToken(ITokenTransition<T> tokenTransition, Expression state)
        {
            Expression caseForToken;

            if (tokenTransition.StateTransitions != null)
            {
                // add switch cases for each state
                var stateCases =
                    tokenTransition.StateTransitions.Select(
                        transition =>
                        Expression.SwitchCase(transition.Body(state), Expression.Constant(transition.State)));

                caseForToken = Expression.Switch(state, tokenTransition.Default(state), stateCases.ToArray());
            }
            else
            {
                caseForToken = tokenTransition.Default(state);
            }

            var tokenCase = Expression.SwitchCase(caseForToken, Expression.Constant(tokenTransition.Token));

            return tokenCase;
        }

        #endregion
    }
}
