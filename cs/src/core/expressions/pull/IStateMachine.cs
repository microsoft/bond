// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions.Pull
{
    using System.Collections.Generic;
    using System.Linq.Expressions;
    
    /// <summary>
    /// Defines the processing logic of a specific input protocol for parsing a specific type schema.
    /// </summary>
    /// <typeparam name="T">The type of current tokens that the reader can pull. </typeparam>
    /// <remarks>
    /// The state machine processing is done by running a loop on an input reader that follows the pattern of
    /// XmlReader. Each iteration switches on the current node type, and then switches on the current state.
    /// Each transition can generate an expression representing logic that will be executed in that transition.
    /// </remarks>
    public interface IStateMachine<T> 
    {
        /// <summary>
        /// Initial state to set when processing this input.
        /// </summary>
        byte InitialState { get; }

        /// <summary>
        /// When in this state, the processing loop terminates.
        /// </summary>
        byte FinalState { get; }

        /// <summary>
        /// Gets tokens that should be ignored when encountered by the "main" outer loop processing the input.
        /// </summary>
        IEnumerable<T> IgnoredTokens { get; }

        /// <summary>
        /// Gets the possible transitions for each handled token.
        /// </summary>
        IEnumerable<ITokenTransition<T>> TokenTransitions { get; }

        /// <summary>
        /// Generates an expression implementing the default logic for encountering an unhandled token.
        /// </summary>
        Expression Default(Expression state);
    }

    /// <summary>
    /// Represents the state machine transitions that should occur for one token.
    /// </summary>
    public interface ITokenTransition<T> 
    {
        /// <summary>
        /// Gets the token that this ITokenTransition represents.
        /// </summary>
        T Token { get; }

        /// <summary>
        /// Gets the different possible transitions for each handled states.
        /// </summary>
        IEnumerable<IStateTransition> StateTransitions { get; }

        /// <summary>
        /// Generates an expression implementing the default logic for this token, for unhandled state.
        /// </summary>
        Expression Default(Expression state);
    }

    /// <summary>
    /// Represents a specific state transition.
    /// </summary>
    public interface IStateTransition
    {
        /// <summary>
        /// Specific state value indicating when this transition should occur.
        /// </summary>
        byte State { get; }

        /// <summary>
        /// Generates an expression implementing the transition's logic.
        /// </summary>
        Expression Body(Expression state);
    }
}
