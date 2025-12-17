:- consult(translator).

test_triple_nested :-
    format('~n=== Test: Triple Nested Findall ===~n'),
    Input = 'test(R) :- findall([Z,Z], (findall([Y,Y], (findall(X, base(X), Xs), member(Y, Xs)), Ys), member(Z, Ys)), R).',
    format('Input: ~w~n~n', [Input]),
    (   translate(Input, Output)
    ->  format('Output:~n~w~n', [Output]),
        format('✓ Test passed~n')
    ;   format('✗ Test FAILED~n')
    ).

test_multiple_findalls_same_level :-
    format('~n=== Test: Multiple Findalls at Same Level ===~n'),
    Input = 'test(R1, R2) :- findall(X, base1(X), R1), findall(Y, base2(Y), R2).',
    format('Input: ~w~n~n', [Input]),
    (   translate(Input, Output)
    ->  format('Output:~n~w~n', [Output]),
        format('✓ Test passed~n')
    ;   format('✗ Test FAILED~n')
    ).

test_code_before_findall :-
    format('~n=== Test: Code Before Findall ===~n'),
    Input = 'test(R) :- init_data(D), findall(f(X), (member(X, D), X > 0), R).',
    format('Input: ~w~n~n', [Input]),
    (   translate(Input, Output)
    ->  format('Output:~n~w~n', [Output]),
        format('✓ Test passed~n')
    ;   format('✗ Test FAILED~n')
    ).

test_code_after_findall :-
    format('~n=== Test: Code After Findall ===~n'),
    Input = 'test(R, Sum) :- findall(X, base(X), R), sum_list(R, Sum).',
    format('Input: ~w~n~n', [Input]),
    (   translate(Input, Output)
    ->  format('Output:~n~w~n', [Output]),
        format('✓ Test passed~n')
    ;   format('✗ Test FAILED~n')
    ).

test_code_between_findalls :-
    format('~n=== Test: Code Between Findalls ===~n'),
    Input = 'test(R2) :- findall(X, base1(X), R1), process(R1, P), findall(Y, member(Y, P), R2).',
    format('Input: ~w~n~n', [Input]),
    (   translate(Input, Output)
    ->  format('Output:~n~w~n', [Output]),
        format('✓ Test passed~n')
    ;   format('✗ Test FAILED~n')
    ).

run_tests :-
    format('~n========================================~n'),
    format('Advanced Test Cases~n'),
    format('========================================~n'),
    catch(test_triple_nested, E, format('Triple nested error: ~w~n', [E])),
    catch(test_multiple_findalls_same_level, E, format('Multiple findalls error: ~w~n', [E])),
    catch(test_code_before_findall, E, format('Code before error: ~w~n', [E])),
    catch(test_code_after_findall, E, format('Code after error: ~w~n', [E])),
    catch(test_code_between_findalls, E, format('Code between error: ~w~n', [E])),
    format('~n========================================~n'),
    format('Tests Completed~n'),
    format('========================================~n~n').

:- initialization(run_tests, main).
