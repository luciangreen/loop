/*
 * example_usage.pl - Examples of using the translator
 * 
 * This file demonstrates how to use the translator to convert
 * findall-based code to explicit recursive predicates.
 * 
 * To use:
 *   ?- consult(translator).
 *   ?- consult(example_usage).
 *   ?- example1.
 *   ?- example2.
 *   ?- example3.
 */

:- consult(translator).

/*
 * Example 1: Simple transformation
 */
example1 :-
    format('~n========================================~n'),
    format('Example 1: Simple Transformation~n'),
    format('========================================~n~n'),
    
    Input = 'simple(R) :- findall(item(X), data(X), R).',
    
    format('INPUT:~n~w~n~n', [Input]),
    translate(Input, Output),
    format('OUTPUT:~n~w~n', [Output]).

/*
 * Example 2: Transformation with assignment
 */
example2 :-
    format('~n========================================~n'),
    format('Example 2: Transformation with Assignment~n'),
    format('========================================~n~n'),
    
    Input = 'transform(R) :- findall(Y, (number(X), Y = double(X)), R).',
    
    format('INPUT:~n~w~n~n', [Input]),
    translate(Input, Output),
    format('OUTPUT:~n~w~n', [Output]).

/*
 * Example 3: Nested findall (Problem Statement)
 */
example3 :-
    format('~n========================================~n'),
    format('Example 3: Nested Findall (Problem Statement)~n'),
    format('========================================~n~n'),
    
    Input = 'predicate(YYs) :- findall([Y2,Y2], (findall(Y1, (colour(Y), Y1 = c-Y), Ys), member(Y2, Ys)), YYs).',
    
    format('INPUT:~n'),
    format('predicate(YYs) :-~n'),
    format('    findall([Y2,Y2],~n'),
    format('        (findall(Y1,(colour(Y),Y1=c-Y),Ys),~n'),
    format('         member(Y2,Ys)),~n'),
    format('        YYs).~n~n'),
    
    translate(Input, Output),
    format('OUTPUT:~n~w~n', [Output]).

/*
 * Run all examples
 */
run_examples :-
    format('~n################################################~n'),
    format('#                                              #~n'),
    format('#  Findall to Predicates Translator Examples  #~n'),
    format('#                                              #~n'),
    format('################################################~n'),
    
    example1,
    example2,
    example3,
    
    format('~n################################################~n'),
    format('#  All examples completed!                    #~n'),
    format('################################################~n~n').

% Entry point
:- initialization(run_examples, main).
