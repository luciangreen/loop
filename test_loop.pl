/*
 * test_loop.pl - Test cases for loop.pl
 *
 * Tests the loop/3 predicate to ensure it correctly converts
 * findall/3 operations to loop-based implementations.
 */

:- use_module(loop).

/**
 * Test helper to run a test and report results
 */
test(TestName, Goal) :-
    format('Testing ~w... ', [TestName]),
    (   call(Goal)
    ->  format('PASSED~n')
    ;   format('FAILED~n'),
        fail
    ).

/**
 * Test 1: Basic list membership collection
 */
test_basic_membership :-
    loop(X, member(X, [1,2,3]), Result),
    Result = [1,2,3].

/**
 * Test 2: Empty result
 */
test_empty_result :-
    loop(X, (member(X, [1,2,3]), X > 10), Result),
    Result = [].

/**
 * Test 3: Filtered collection
 */
test_filtered_collection :-
    loop(X, (member(X, [1,2,3,4,5]), X > 2), Result),
    Result = [3,4,5].

/**
 * Test 4: Template with compound terms
 */
test_compound_template :-
    loop(pair(X,Y), (member(X, [1,2]), member(Y, [a,b])), Result),
    Result = [pair(1,a), pair(1,b), pair(2,a), pair(2,b)].

/**
 * Test 5: Arithmetic expressions
 */
test_arithmetic :-
    loop(Y, (member(X, [1,2,3]), Y is X * 2), Result),
    Result = [2,4,6].

/**
 * Test 6: Single solution
 */
test_single_solution :-
    loop(X, (member(X, [5,6,7]), X = 6), Result),
    Result = [6].

/**
 * Test 7: Comparison with findall/3
 */
test_comparison_with_findall :-
    % Using loop/3
    loop(X, member(X, [a,b,c,d]), LoopResult),
    % Using findall/3
    findall(X, member(X, [a,b,c,d]), FindallResult),
    % Should produce same result
    LoopResult = FindallResult.

/**
 * Test 8: Nested goals
 */
test_nested_goals :-
    loop(X-Y, (member(X, [1,2]), member(Y, [3,4])), Result),
    length(Result, 4),
    member(1-3, Result),
    member(1-4, Result),
    member(2-3, Result),
    member(2-4, Result).

/**
 * Run all tests
 */
run_all_tests :-
    format('~n=== Running Loop Tests ===~n~n'),
    test('Basic membership', test_basic_membership),
    test('Empty result', test_empty_result),
    test('Filtered collection', test_filtered_collection),
    test('Compound template', test_compound_template),
    test('Arithmetic', test_arithmetic),
    test('Single solution', test_single_solution),
    test('Comparison with findall', test_comparison_with_findall),
    test('Nested goals', test_nested_goals),
    format('~n=== All tests completed ===~n').

/**
 * Entry point - run tests when loaded
 */
:- initialization(run_all_tests, main).
