/*
 * test_loop_alt.pl - Test the alternative implementation loop_alt/3
 */

:- use_module(loop).

test_loop_alt :-
    format('Testing loop_alt/3 implementation...~n'),
    
    % Test 1: Basic usage
    loop_alt(X, member(X, [1,2,3]), List1),
    format('Test 1 - Basic: ~w~n', [List1]),
    (List1 = [1,2,3] -> format('  PASSED~n') ; format('  FAILED~n')),
    
    % Test 2: Filtered
    loop_alt(X, (member(X, [1,2,3,4,5]), X > 2), List2),
    format('Test 2 - Filtered: ~w~n', [List2]),
    (List2 = [3,4,5] -> format('  PASSED~n') ; format('  FAILED~n')),
    
    % Test 3: Compound terms
    loop_alt(X-Y, (member(X, [a,b]), member(Y, [1,2])), List3),
    format('Test 3 - Compound: ~w~n', [List3]),
    (length(List3, 4) -> format('  PASSED~n') ; format('  FAILED~n')),
    
    format('~nAll loop_alt tests completed!~n').

:- initialization(test_loop_alt, main).
