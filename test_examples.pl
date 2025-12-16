/*
 * test_examples.pl - Additional test cases for findall conversion
 * 
 * This file contains various test cases that demonstrate the transformation
 * from nested findall operations to explicit recursive predicates.
 */

% ============================================================================
% Test Case 1: Simple single-level transformation
% ============================================================================

% Base data
fruits([apple, banana, cherry]).

% Converts each fruit to prefixed form: fruit-X
test_simple(Result) :-
    fruits(Fruits1),
    transform001(Fruits1, Result).

% Transformation: X -> fruit-X
transform001([],[]).
transform001([X|Xs],[fruit-X|Ys]) :-
    transform001(Xs,Ys).

test1 :-
    format('~n=== Test 1: Simple Transformation ===~n'),
    format('Input: fruits([apple, banana, cherry])~n'),
    format('Transform: X -> fruit-X~n'),
    test_simple(Result),
    format('Result: ~w~n', [Result]),
    Expected = [fruit-apple, fruit-banana, fruit-cherry],
    (   Result = Expected
    ->  format('✓ Test 1 PASSED~n')
    ;   format('✗ Test 1 FAILED~n'),
        format('Expected: ~w~n', [Expected])
    ).

% ============================================================================
% Test Case 2: Double each element
% ============================================================================

numbers([1, 2, 3]).

test_double(Result) :-
    numbers(Numbers1),
    transform002(Numbers1, Result).

% Transformation: X -> X*2
transform002([],[]).
transform002([X|Xs],[Y|Ys]) :-
    Y is X * 2,
    transform002(Xs,Ys).

test2 :-
    format('~n=== Test 2: Arithmetic Transformation ===~n'),
    format('Input: numbers([1, 2, 3])~n'),
    format('Transform: X -> X*2~n'),
    test_double(Result),
    format('Result: ~w~n', [Result]),
    Expected = [2, 4, 6],
    (   Result = Expected
    ->  format('✓ Test 2 PASSED~n')
    ;   format('✗ Test 2 FAILED~n'),
        format('Expected: ~w~n', [Expected])
    ).

% ============================================================================
% Test Case 3: List wrapping
% ============================================================================

items([a, b, c]).

test_wrap(Result) :-
    items(Items1),
    transform003(Items1, Result).

% Transformation: X -> [X]
transform003([],[]).
transform003([X|Xs],[[X]|Ys]) :-
    transform003(Xs,Ys).

test3 :-
    format('~n=== Test 3: List Wrapping ===~n'),
    format('Input: items([a, b, c])~n'),
    format('Transform: X -> [X]~n'),
    test_wrap(Result),
    format('Result: ~w~n', [Result]),
    Expected = [[a], [b], [c]],
    (   Result = Expected
    ->  format('✓ Test 3 PASSED~n')
    ;   format('✗ Test 3 FAILED~n'),
        format('Expected: ~w~n', [Expected])
    ).

% ============================================================================
% Test Case 4: Nested transformation (2 levels)
% ============================================================================

values([10, 20]).

test_nested(Result) :-
    values(Values1),
    transform004a(Values1, Values2),
    transform004b(Values2, Result).

% First transformation: X -> X + 5
transform004a([],[]).
transform004a([X|Xs],[Y|Ys]) :-
    Y is X + 5,
    transform004a(Xs,Ys).

% Second transformation: X -> [X, X]
transform004b([],[]).
transform004b([X|Xs],[[X,X]|Ys]) :-
    transform004b(Xs,Ys).

test4 :-
    format('~n=== Test 4: Two-Level Nested Transformation ===~n'),
    format('Input: values([10, 20])~n'),
    format('Transform 1: X -> X + 5~n'),
    format('Transform 2: X -> [X, X]~n'),
    test_nested(Result),
    format('Result: ~w~n', [Result]),
    Expected = [[15, 15], [25, 25]],
    (   Result = Expected
    ->  format('✓ Test 4 PASSED~n')
    ;   format('✗ Test 4 FAILED~n'),
        format('Expected: ~w~n', [Expected])
    ).

% ============================================================================
% Test Case 5: The original example from problem statement
% ============================================================================

colours([red, yellow]).

test_original(Result) :-
    colours(Colours1),
    transform005a(Colours1, Colours2),
    transform005b(Colours2, Result).

% First transformation: X -> c-X
transform005a([],[]).
transform005a([X1|Xs],[X2|Ys]):-
    X2=c-X1,
    transform005a(Xs,Ys).

% Second transformation: X -> [X, X]
transform005b([],[]).
transform005b([X|Xs],[[X,X]|Ys]):-
    transform005b(Xs,Ys).

test5 :-
    format('~n=== Test 5: Original Problem Statement Example ===~n'),
    format('Input: colours([red, yellow])~n'),
    format('Transform 1: X -> c-X~n'),
    format('Transform 2: X -> [X, X]~n'),
    test_original(Result),
    format('Result: ~w~n', [Result]),
    Expected = [[c-red, c-red], [c-yellow, c-yellow]],
    (   Result = Expected
    ->  format('✓ Test 5 PASSED~n')
    ;   format('✗ Test 5 FAILED~n'),
        format('Expected: ~w~n', [Expected])
    ).

% ============================================================================
% Test Case 6: Triple nesting
% ============================================================================

base([x, y]).

test_triple(Result) :-
    base(Base1),
    transform006a(Base1, Base2),
    transform006b(Base2, Base3),
    transform006c(Base3, Result).

% Transform 1: X -> f(X)
transform006a([],[]).
transform006a([X|Xs],[f(X)|Ys]) :-
    transform006a(Xs,Ys).

% Transform 2: X -> [X, X]
transform006b([],[]).
transform006b([X|Xs],[[X,X]|Ys]) :-
    transform006b(Xs,Ys).

% Transform 3: X -> pair(X)
transform006c([],[]).
transform006c([X|Xs],[pair(X)|Ys]) :-
    transform006c(Xs,Ys).

test6 :-
    format('~n=== Test 6: Three-Level Nested Transformation ===~n'),
    format('Input: base([x, y])~n'),
    format('Transform 1: X -> f(X)~n'),
    format('Transform 2: X -> [X, X]~n'),
    format('Transform 3: X -> pair(X)~n'),
    test_triple(Result),
    format('Result: ~w~n', [Result]),
    Expected = [pair([f(x), f(x)]), pair([f(y), f(y)])],
    (   Result = Expected
    ->  format('✓ Test 6 PASSED~n')
    ;   format('✗ Test 6 FAILED~n'),
        format('Expected: ~w~n', [Expected])
    ).

% ============================================================================
% Run all tests
% ============================================================================

run_all_tests :-
    format('~n========================================~n'),
    format('  Findall Conversion Test Suite~n'),
    format('========================================~n'),
    test1,
    test2,
    test3,
    test4,
    test5,
    test6,
    format('~n========================================~n'),
    format('  All tests completed!~n'),
    format('========================================~n~n').

% Entry point
:- initialization(run_all_tests, main).
