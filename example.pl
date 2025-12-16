/*
 * example.pl - Example of converting nested findall to explicit predicates
 * 
 * This demonstrates how nested findall operations can be converted to
 * explicit recursive predicates that operate on lists instead of using findall.
 * 
 * Original version (using nested findall):
 * colour(red).
 * colour(yellow).
 * predicate(YYs):-findall([Y2,Y2],(findall(Y1,(colour(Y),Y1=c-Y),Ys),member(Y2,Ys)),YYs).
 * 
 * Converted version (without findall):
 * colours([red,yellow]).
 * 
 * predicate(Colours3) :-
 *     colours(Colours1),
 *     findall001(Colours1,Colours2),
 *     findall002(Colours2,Colours3).
 * 
 * findall001([],[]).
 * findall001([X1|Xs],[X2|Ys]):-
 *     X2=c-X1,
 *     findall001(Xs,Ys).
 * 
 * findall002([],[]).
 * findall002([X|Xs],[[X,X]|Ys]):-
 *     findall002(Xs,Ys).
 */

% Base data as a list fact (converted from colour(red), colour(yellow))
colours([red,yellow]).

% Main predicate - chains the transformation predicates
predicate(Colours3) :-
    colours(Colours1),
    findall001(Colours1,Colours2),
    findall002(Colours2,Colours3).

% First transformation: converts each colour X to c-X
% This replaces: findall(Y1,(colour(Y),Y1=c-Y),Ys)
findall001([],[]).
findall001([X1|Xs],[X2|Ys]):-
    X2=c-X1,
    findall001(Xs,Ys).

% Second transformation: duplicates each element into a list [X,X]
% This replaces: findall([Y2,Y2],(member(Y2,Ys)),YYs)
findall002([],[]).
findall002([X|Xs],[[X,X]|Ys]):-
    findall002(Xs,Ys).

% Test predicate to verify the result
test_example :-
    predicate(Result),
    format('predicate(A).~n'),
    format('A = ~w~n', [Result]),
    (   Result = [[c-red, c-red], [c-yellow, c-yellow]]
    ->  format('✓ Test PASSED~n')
    ;   format('✗ Test FAILED~n'),
        format('Expected: [[c-red, c-red], [c-yellow, c-yellow]]~n'),
        format('Got: ~w~n', [Result])
    ).

% Entry point
:- initialization(test_example, main).
