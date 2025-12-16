/*
 * translator.pl - Automatic translator from nested findall to explicit predicates
 * 
 * This program automatically converts Prolog code that uses nested findall/3
 * operations into equivalent code using explicit recursive predicates.
 * 
 * Main predicate:
 *   translate(+InputClause, -OutputCode)
 * 
 * Example usage:
 *   ?- translate('predicate(YYs):-findall([Y2,Y2],(findall(Y1,(colour(Y),Y1=c-Y),Ys),member(Y2,Ys)),YYs).', Output).
 * 
 * The translator performs the following steps:
 * 1. Parses the nested findall structure
 * 2. Identifies base predicates and transformation logic
 * 3. Generates numbered recursive predicates (findall001, findall002, etc.)
 * 4. Generates the main predicate that chains all transformations
 */

:- use_module(library(lists)).

/*
 * translate(+InputString, -OutputString)
 * Main translation entry point
 */
translate(InputString, OutputString) :-
    % Read the input as a term
    read_term_from_atom(InputString, (Head :- Body), []),
    Head =.. [PredName|[_ResultVar]],
    
    % Parse findall structure
    parse_findall_structure(Body, Structure),
    
    % Generate output components
    generate_base_fact(Structure, BaseFactLine),
    generate_main_predicate(PredName, Structure, MainPredLine),
    generate_helper_predicates(Structure, 1, HelperLines),
    
    % Format output
    format_output(BaseFactLine, MainPredLine, HelperLines, OutputString).

/*
 * parse_findall_structure(+Body, -Structure)
 * Parses nested findall into a structured format
 */
parse_findall_structure(findall(Template, Goal, _), Structure) :-
    (   has_inner_findall(Goal, InnerFindall, OuterGoal)
    ->  parse_findall_structure(InnerFindall, InnerStructure),
        Structure = nested(InnerStructure, Template, OuterGoal)
    ;   % Base case - innermost findall
        parse_base_findall(Template, Goal, Structure)
    ).

/*
 * has_inner_findall(+Goal, -InnerFindall, -OuterGoal)
 * Checks if goal contains an inner findall
 */
has_inner_findall((findall(T,G,R), Rest), findall(T,G,R), Rest) :- !.
has_inner_findall((Rest, findall(T,G,R)), findall(T,G,R), Rest) :- !.

/*
 * parse_base_findall(+Template, +Goal, -Structure)
 * Parses the innermost (base) findall
 */
parse_base_findall(Template, (BasePred, Transform), base(BasePred, Template, Transform)) :- !.
parse_base_findall(Template, BasePred, base(BasePred, Template, none)).

/*
 * generate_base_fact(+Structure, -BaseFactLine)
 * Generates the base fact line
 */
generate_base_fact(Structure, Line) :-
    get_base_predicate_name(Structure, BaseName),
    atom_concat(BaseName, 's', PluralName),
    format(atom(Line), '% ~w([...]).  % TODO: Fill in your base facts', [PluralName]).

get_base_predicate_name(base(BasePred, _, _), Name) :-
    BasePred =.. [Name|_].
get_base_predicate_name(nested(Inner, _, _), Name) :-
    get_base_predicate_name(Inner, Name).

/*
 * generate_main_predicate(+PredName, +Structure, -MainPredLine)
 * Generates the main predicate that chains transformations
 */
generate_main_predicate(PredName, Structure, Line) :-
    get_base_predicate_name(Structure, BaseName),
    atom_concat(BaseName, 's', BasePluralName),
    capitalize_atom(BasePluralName, CapBase),
    
    count_levels(Structure, NumLevels),
    NumVars is NumLevels + 1,
    
    generate_var_sequence(CapBase, NumVars, VarSeq),
    VarSeq = [FirstVar|RestVars],
    last(VarSeq, LastVar),
    
    format(atom(BaseCall), '    ~w(~w)', [BasePluralName, FirstVar]),
    generate_chain_calls(RestVars, 1, FirstVar, ChainCalls),
    
    append([BaseCall], ChainCalls, AllCalls),
    atomics_to_string(AllCalls, ',\n', BodyStr),
    
    format(atom(Line), '~w(~w) :-\n~w.', [PredName, LastVar, BodyStr]).

/*
 * count_levels(+Structure, -Count)
 * Counts the number of findall levels
 */
count_levels(base(_, _, _), 1).
count_levels(nested(Inner, _, _), Count) :-
    count_levels(Inner, InnerCount),
    Count is InnerCount + 1.

/*
 * generate_var_sequence(+BaseName, +N, -Sequence)
 * Generates variable sequence: [Base1, Base2, ..., BaseN]
 */
generate_var_sequence(BaseName, N, Sequence) :-
    findall(Var, (between(1, N, I), format(atom(Var), '~w~d', [BaseName, I])), Sequence).

/*
 * generate_chain_calls(+Vars, +StartN, +PrevVar, -Calls)
 * Generates the chain of findallXXX calls
 */
generate_chain_calls([], _, _, []).
generate_chain_calls([NextVar|Rest], N, PrevVar, [Call|RestCalls]) :-
    format(atom(PredName), 'findall~|~`0t~d~3+', [N]),
    format(atom(Call), '    ~w(~w, ~w)', [PredName, PrevVar, NextVar]),
    N1 is N + 1,
    generate_chain_calls(Rest, N1, NextVar, RestCalls).

/*
 * generate_helper_predicates(+Structure, +StartN, -Lines)
 * Generates all helper predicates (findall001, findall002, etc.)
 */
generate_helper_predicates(base(BasePred, Template, Transform), N, [Line1, Line2]) :-
    format(atom(PredName), 'findall~|~`0t~d~3+', [N]),
    format(atom(Line1), '~w([],[]).', [PredName]),
    generate_recursive_clause(PredName, Template, Transform, BasePred, Line2).

generate_helper_predicates(nested(Inner, Template, _OuterGoal), StartN, AllLines) :-
    generate_helper_predicates(Inner, StartN, InnerLines),
    count_levels(Inner, InnerLevels),
    N is StartN + InnerLevels,
    format(atom(PredName), 'findall~|~`0t~d~3+', [N]),
    format(atom(Line1), '~w([],[]).', [PredName]),
    generate_outer_recursive_clause(PredName, Template, Line2),
    append(InnerLines, [Line1, Line2], AllLines).

/*
 * generate_recursive_clause(+PredName, +Template, +Transform, +BasePred, -Clause)
 * Generates the recursive clause for base-level transformation
 */
generate_recursive_clause(PredName, _Template, (_ = Expr), _BasePred, Clause) :-
    % Handle assignment: Y1 = c-Y
    % Check if expression is of form c-Var or similar
    (   Expr = ('-'(Prefix, _))
    ->  format(atom(Clause), '~w([X|Xs],[Y|Ys]) :- Y = ~w-X, ~w(Xs,Ys).', 
               [PredName, Prefix, PredName])
    ;   Expr =.. [Functor|_]
    ->  format(atom(Clause), '~w([X|Xs],[Y|Ys]) :- Y = ~w(X), ~w(Xs,Ys).', 
               [PredName, Functor, PredName])
    ;   format(atom(Clause), '~w([X|Xs],[Y|Ys]) :- Y = X, ~w(Xs,Ys).', 
               [PredName, PredName])
    ).

generate_recursive_clause(PredName, Template, none, _BasePred, Clause) :-
    % For templates with functors, replace variables with X
    (   compound(Template),
        \+ is_list(Template)
    ->  Template =.. [Functor|_],
        format(atom(Clause), '~w([X|Xs],[~w(X)|Ys]) :- ~w(Xs,Ys).', 
               [PredName, Functor, PredName])
    ;   format_template(Template, TemplateStr),
        format(atom(Clause), '~w([X|Xs],[~w|Ys]) :- ~w(Xs,Ys).', 
               [PredName, TemplateStr, PredName])
    ).

/*
 * generate_outer_recursive_clause(+PredName, +Template, -Clause)
 * Generates recursive clause for outer transformations
 */
generate_outer_recursive_clause(PredName, Template, Clause) :-
    format_template(Template, TemplateStr),
    format(atom(Clause), '~w([X|Xs],[~w|Ys]) :- ~w(Xs,Ys).', 
           [PredName, TemplateStr, PredName]).

/*
 * format_template(+Template, -TemplateStr)
 * Formats a template for output
 */
format_template([A,B], TemplateStr) :-
    A == B,
    !,
    TemplateStr = '[X,X]'.
format_template([A|Rest], TemplateStr) :-
    !,
    format(atom(TemplateStr), '[~w|~w]', [A, Rest]).
format_template(Template, TemplateStr) :-
    format(atom(TemplateStr), '~w', [Template]).

/*
 * capitalize_atom(+Atom, -CapitalizedAtom)
 * Capitalizes the first letter of an atom
 */
capitalize_atom(Atom, Result) :-
    atom_chars(Atom, [First|Rest]),
    upcase_atom(First, FirstUp),
    atom_chars(Result, [FirstUp|Rest]).

/*
 * format_output(+BaseFact, +MainPred, +HelperPreds, -OutputString)
 * Formats all components into final output string
 */
format_output(BaseFact, MainPred, HelperPreds, Output) :-
    atomics_to_string([BaseFact, '', MainPred, ''], '\n', Part1),
    atomics_to_string(HelperPreds, '\n', Part2),
    format(atom(Output), '~w~w', [Part1, Part2]).

/*
 * Test cases
 */

test_simple :-
    format('~n=== Test 1: Simple Single-Level Findall ===~n'),
    Input = 'test(R) :- findall(f(X), base(X), R).',
    format('Input:  ~w~n~n', [Input]),
    translate(Input, Output),
    format('Output:~n~w~n', [Output]),
    format('✓ Test 1 completed~n').

test_with_transform :-
    format('~n=== Test 2: Single Findall with Transform ===~n'),
    Input = 'test(R) :- findall(Y1, (colour(Y), Y1 = c-Y), R).',
    format('Input:  ~w~n~n', [Input]),
    translate(Input, Output),
    format('Output:~n~w~n', [Output]),
    format('✓ Test 2 completed~n').

test_nested :-
    format('~n=== Test 3: Nested Findall (Problem Statement) ===~n'),
    Input = 'predicate(YYs) :- findall([Y2,Y2], (findall(Y1, (colour(Y), Y1 = c-Y), Ys), member(Y2, Ys)), YYs).',
    format('Input:~n'),
    format('predicate(YYs) :-~n'),
    format('    findall([Y2,Y2],~n'),
    format('        (findall(Y1,(colour(Y),Y1=c-Y),Ys),~n'),
    format('         member(Y2,Ys)),~n'),
    format('        YYs).~n~n'),
    translate(Input, Output),
    format('Output:~n~w~n', [Output]),
    format('✓ Test 3 completed~n').

run_tests :-
    format('~n================================================~n'),
    format('  Findall to Predicates Translator~n'),
    format('================================================~n'),
    catch(test_simple, E, format('Test 1 error: ~w~n', [E])),
    catch(test_with_transform, E, format('Test 2 error: ~w~n', [E])),
    catch(test_nested, E, format('Test 3 error: ~w~n', [E])),
    format('~n================================================~n'),
    format('  All Tests Completed!~n'),
    format('================================================~n~n').

% Entry point
:- initialization(run_tests, main).
