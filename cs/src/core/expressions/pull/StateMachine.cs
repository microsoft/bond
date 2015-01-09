// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions.Pull
{
    using System.Collections.Generic;
    using System.Linq.Expressions;

    internal delegate Expression TransitionBody(Expression state);

    internal class StateMachine<T> : IStateMachine<T>
    {
        public byte InitialState { get; set; }
        
        public byte FinalState { get; set; }
        
        public IEnumerable<T> IgnoredTokens { get; set; }

        public IEnumerable<ITokenTransition<T>> TokenTransitions { get; set; }

        public TransitionBody Default { private get; set; }
        
        Expression IStateMachine<T>.Default(Expression state)
        {
            return Default == null ? Expression.Empty() : Default(state);
        }
    }

    internal class TokenTransition<T> : ITokenTransition<T> 
    {
        public T Token { get; set; }
        
        public IEnumerable<IStateTransition> StateTransitions { get; set; }

        public TransitionBody Default { private get; set; }
        
        Expression ITokenTransition<T>.Default(Expression state)
        {
            return Default == null ? Expression.Empty() : Default(state);
        }
    }


    internal class StateTransition : IStateTransition
    {
        public StateTransition(byte state, TransitionBody body)
        {
            State = state;
            Body = s => Expression.Block(
                body(s),
                Expression.Empty());
        }

        public StateTransition(byte currentState, byte nextState, TransitionBody body)
        {
            State = currentState;
            Body = state => Expression.Block(
                body(state),
                Expression.Assign(state, Expression.Constant(nextState)),
                Expression.Empty());
        }

        public byte State { get; private set; }
        
        TransitionBody Body { get; set; }
        
        Expression IStateTransition.Body(Expression state)
        {
            return Body == null ? Expression.Empty() : Body(state);
        }
    }
}
