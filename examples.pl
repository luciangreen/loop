/*
 * examples.pl - Example usage of loop/3
 *
 * This file demonstrates various ways to use loop/3 as an alternative
 * to findall/3, showing how it converts findall operations to explicit
 * loop-based implementations.
 */

:- use_module(loop).

/**
 * Example 1: Basic usage - collecting list elements
 */
example1 :-
    format('~nExample 1: Basic list membership~n'),
    format('Goal: loop(X, member(X, [1,2,3]), List)~n'),
    loop(X, member(X, [1,2,3]), List),
    format('Result: ~w~n', [List]).

/**
 * Example 2: Filtering with conditions
 */
example2 :-
    format('~nExample 2: Filtering with conditions~n'),
    format('Goal: loop(X, (member(X, [1,2,3,4,5,6]), X mod 2 =:= 0), List)~n'),
    loop(X, (member(X, [1,2,3,4,5,6]), X mod 2 =:= 0), List),
    format('Result (even numbers): ~w~n', [List]).

/**
 * Example 3: Collecting compound terms
 */
example3 :-
    format('~nExample 3: Collecting compound terms~n'),
    format('Goal: loop(person(Name,Age), person(Name,Age), List)~n'),
    % Define some facts
    assertz(person(alice, 30)),
    assertz(person(bob, 25)),
    assertz(person(charlie, 35)),
    loop(person(Name,Age), person(Name,Age), List),
    format('Result: ~w~n', [List]),
    % Cleanup
    retractall(person(_,_)).

/**
 * Example 4: Arithmetic transformations
 */
example4 :-
    format('~nExample 4: Arithmetic transformations~n'),
    format('Goal: loop(Y, (member(X, [1,2,3,4]), Y is X * X), List)~n'),
    loop(Y, (member(X, [1,2,3,4]), Y is X * X), List),
    format('Result (squares): ~w~n', [List]).

/**
 * Example 5: Cartesian product
 */
example5 :-
    format('~nExample 5: Cartesian product~n'),
    format('Goal: loop(X-Y, (member(X, [a,b]), member(Y, [1,2])), List)~n'),
    loop(X-Y, (member(X, [a,b]), member(Y, [1,2])), List),
    format('Result: ~w~n', [List]).

/**
 * Example 6: Comparison with findall/3
 */
example6 :-
    format('~nExample 6: Comparison with findall/3~n'),
    TestList = [apple, banana, cherry, date],
    
    format('Using findall/3:~n'),
    findall(X, member(X, TestList), FindallResult),
    format('  Result: ~w~n', [FindallResult]),
    
    format('Using loop/3:~n'),
    loop(X, member(X, TestList), LoopResult),
    format('  Result: ~w~n', [LoopResult]),
    
    (   FindallResult = LoopResult
    ->  format('  ✓ Results are identical!~n')
    ;   format('  ✗ Results differ~n')
    ).

/**
 * Example 7: Empty result case
 */
example7 :-
    format('~nExample 7: Empty result (no solutions)~n'),
    format('Goal: loop(X, (member(X, [1,2,3]), X > 100), List)~n'),
    loop(X, (member(X, [1,2,3]), X > 100), List),
    format('Result: ~w~n', [List]).

/**
 * Example 8: Range generation
 */
example8 :-
    format('~nExample 8: Range generation~n'),
    format('Goal: loop(X, between(1, 10, X), List)~n'),
    loop(X, between(1, 10, X), List),
    format('Result: ~w~n', [List]).

/**
 * Run all examples
 */
run_all_examples :-
    format('~n======================================~n'),
    format('  Loop/3 Usage Examples~n'),
    format('======================================~n'),
    example1,
    example2,
    example3,
    example4,
    example5,
    example6,
    example7,
    example8,
    format('~n======================================~n'),
    format('  All examples completed!~n'),
    format('======================================~n~n').

% Entry point
:- initialization(run_all_examples, main).
