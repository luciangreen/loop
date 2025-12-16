/*
 * loop.pl - Converts Prolog findall/3 to a loop with predicates
 * 
 * This module provides an alternative implementation to findall/3
 * using explicit loop predicates instead of the built-in findall.
 * 
 * Main predicate:
 *   loop(+Template, +Goal, -List)
 *     Equivalent to findall(Template, Goal, List)
 *     Collects all solutions of Template for which Goal succeeds
 *     using an iterative loop mechanism with backtracking.
 *
 * Usage example:
 *   ?- loop(X, member(X, [1,2,3]), List).
 *   List = [1, 2, 3].
 */

:- module(loop, [loop/3, loop_alt/3]).

/**
 * loop(+Template, +Goal, -List)
 * 
 * Collects all solutions to Goal, instantiating Template for each solution.
 * This is functionally equivalent to findall(Template, Goal, List) but
 * implemented using explicit loop predicates with backtracking.
 *
 * @param Template The term to collect for each solution
 * @param Goal The goal to prove
 * @param List The resulting list of collected solutions
 */
loop(Template, Goal, List) :-
    % Generate unique key for this collection
    gensym(loop_key_, Key),
    % Collect solutions using failure-driven loop
    loop_collect(Key, Template, Goal),
    % Retrieve collected solutions
    loop_retrieve(Key, List).

/**
 * loop_collect(+Key, +Template, +Goal)
 * 
 * Uses a failure-driven loop to collect all solutions.
 * Each solution is asserted to the database with the given key.
 *
 * @param Key Unique identifier for this collection
 * @param Template The term to collect
 * @param Goal The goal to prove
 */
loop_collect(Key, Template, Goal) :-
    % Try each solution via backtracking
    call(Goal),
    % Copy the template to avoid variable binding issues
    copy_term(Template, Copy),
    % Store this solution
    assertz(loop_temp_solution(Key, Copy)),
    % Force backtracking to get next solution
    fail.
loop_collect(_, _, _).
% When all solutions are exhausted, succeed

/**
 * loop_retrieve(+Key, -List)
 * 
 * Retrieves all solutions with the given key and returns them as a list.
 * Cleans up the stored solutions after retrieval.
 *
 * @param Key Unique identifier for the collection
 * @param List The list of collected solutions
 */
loop_retrieve(Key, List) :-
    loop_retrieve_acc(Key, [], List).

/**
 * loop_retrieve_acc(+Key, +Acc, -List)
 * 
 * Helper predicate that retrieves solutions one by one using an accumulator.
 *
 * @param Key Unique identifier for the collection
 * @param Acc Accumulator for building the result list
 * @param List Final list of solutions
 */
loop_retrieve_acc(Key, Acc, List) :-
    % Try to retract one solution
    (   retract(loop_temp_solution(Key, Solution))
    ->  % Solution found, add to accumulator and continue
        loop_retrieve_acc(Key, [Solution|Acc], List)
    ;   % No more solutions, reverse accumulator to get correct order
        reverse(Acc, List)
    ).

% Alternative implementation using explicit iteration with failure-driven loop
/**
 * loop_alt(+Template, +Goal, -List)
 * 
 * Alternative implementation using assert/retract for collecting solutions.
 * This version explicitly demonstrates the loop pattern with side effects.
 */
loop_alt(Template, Goal, List) :-
    % Generate unique key for this collection
    gensym(loop_, Key),
    % Initialize collection
    assert_solutions(Key, Template, Goal),
    % Collect all asserted solutions
    collect_solutions(Key, List),
    % Clean up
    cleanup_solutions(Key).

% Helper for assert-based collection
assert_solutions(Key, Template, Goal) :-
    call(Goal),
    copy_term(Template, Copy),
    assertz(loop_solution(Key, Copy)),
    fail.
assert_solutions(_, _, _).

% Collect all solutions with given key
collect_solutions(Key, List) :-
    collect_solutions_acc(Key, [], List).

% Helper to collect solutions using accumulator
collect_solutions_acc(Key, Acc, List) :-
    (   retract(loop_solution(Key, Solution))
    ->  collect_solutions_acc(Key, [Solution|Acc], List)
    ;   reverse(Acc, List)
    ).

% Clean up asserted solutions
cleanup_solutions(Key) :-
    retractall(loop_solution(Key, _)).

% Dynamic predicates for storing solutions
:- dynamic loop_solution/2.
:- dynamic loop_temp_solution/2.
