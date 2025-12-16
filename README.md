# Findall to Predicates Translator

This repository provides an **automatic translator** that converts Prolog code using nested `findall/3` operations into equivalent code using explicit recursive predicates.

## Overview

The `translator.pl` program automatically analyzes Prolog predicates with nested `findall/3` calls and generates equivalent code using explicit recursive predicates. The translator:

1. **Parses** nested findall structures
2. **Identifies** base predicates and transformation logic  
3. **Generates** numbered recursive predicates (e.g., `findall001`, `findall002`)
4. **Produces** the main predicate that chains all transformations

## Usage

### Interactive Mode
```prolog
?- consult(translator).
?- translate('predicate(YYs):-findall([Y2,Y2],(findall(Y1,(colour(Y),Y1=c-Y),Ys),member(Y2,Ys)),YYs).', Output).
```

### Run Examples
```bash
swipl -g run_examples -t halt example_usage.pl
```

### Run Test Suite
```bash
swipl -g run_tests -t halt translator.pl
```

## Example Transformation

The translator automatically converts code like this:

### Input: Original Code (Using Nested Findall)

```prolog
% Base facts
colour(red).
colour(yellow).

% Predicate with nested findall
predicate(YYs):-
    findall([Y2,Y2],
        (findall(Y1,(colour(Y),Y1=c-Y),Ys),
         member(Y2,Ys)),
        YYs).
```

### Output: Converted Code (Without Findall)

The translator generates:

```prolog
% Base data as a list fact
colours([red,yellow]).

% Main predicate - chains the transformation predicates
predicate(Colours3) :-
    colours(Colours1),
    findall001(Colours1,Colours2),
    findall002(Colours2,Colours3).

% First transformation: converts each colour X to c-X
% Replaces: findall(Y1,(colour(Y),Y1=c-Y),Ys)
findall001([],[]).
findall001([X1|Xs],[X2|Ys]):-
    X2=c-X1,
    findall001(Xs,Ys).

% Second transformation: duplicates each element into a list [X,X]
% Replaces: findall([Y2,Y2],(member(Y2,Ys)),YYs)
findall002([],[]).
findall002([X|Xs],[[X,X]|Ys]):-
    findall002(Xs,Ys).
```

Both versions produce the same result:
```prolog
?- predicate(A).
A = [[c-red, c-red], [c-yellow, c-yellow]].
```

## How the Translator Works

### Step 1: Identify Base Predicates

Scan the innermost `findall` operation to find base predicates that generate data:
- Example: `colour(Y)` in `findall(Y1,(colour(Y),Y1=c-Y),Ys)`

### Step 2: Convert Base Predicates to Lists

Collect all instances of the base predicate and create a list fact:
- From: `colour(red). colour(yellow).`
- To: `colours([red,yellow]).`

The plural form is created by appending 's' to the predicate name.

### Step 3: Analyze Each Findall Level

For each `findall(Template, Goal, Result)` operation from innermost to outermost:

1. **Extract the transformation logic** from the Goal
2. **Identify the template pattern** that determines the output format
3. **Create a numbered predicate** (`findall001`, `findall002`, etc.)

### Step 4: Generate Recursive Predicates

For each findall level, generate two clauses:

**Base case:**
```prolog
findallXXX([],[]).
```

**Recursive case:**
```prolog
findallXXX([InputHead|InputTail],[OutputHead|OutputTail]):-
    % Transformation logic here
    OutputHead = <transformation of InputHead>,
    findallXXX(InputTail,OutputTail).
```

The transformation logic depends on the original `findall` Goal:
- If Goal contains `X=Expr`, create `OutputHead=Expr` with substitution
- If Goal is just `member(X,List)`, create `OutputHead=Template` with substitution

### Step 5: Chain Transformations

Create the main predicate that:
1. Calls the base list fact
2. Chains all numbered predicates in sequence
3. Returns the final result

```prolog
predicate(FinalResult) :-
    base_list_fact(Var1),
    findall001(Var1,Var2),
    findall002(Var2,Var3),
    ...
    findallNNN(VarN,FinalResult).
```

## Transformation Examples

### Example 1: Simple Transformation

**Original:**
```prolog
colour(red).
colour(yellow).
simple(Result) :- findall(c-Y, colour(Y), Result).
```

**Converted:**
```prolog
colours([red,yellow]).

simple(Result) :-
    colours(Colours1),
    findall001(Colours1, Result).

findall001([],[]).
findall001([X|Xs],[c-X|Ys]) :-
    findall001(Xs,Ys).
```

### Example 2: Template Duplication

**Original:**
```prolog
items([a,b,c]).
duplicate(Result) :- items(Xs), findall([X,X], member(X,Xs), Result).
```

**Converted:**
```prolog
items([a,b,c]).

duplicate(Result) :-
    items(Items1),
    findall001(Items1, Result).

findall001([],[]).
findall001([X|Xs],[[X,X]|Ys]) :-
    findall001(Xs,Ys).
```

### Example 3: Nested Findalls (From Problem Statement)

**Original:**
```prolog
colour(red).
colour(yellow).

predicate(YYs):-
    findall([Y2,Y2],
        (findall(Y1,(colour(Y),Y1=c-Y),Ys),
         member(Y2,Ys)),
        YYs).
```

**Analysis:**
- Inner findall: transforms each `colour(Y)` to `c-Y`
- Outer findall: duplicates each element into `[X,X]` format

**Converted:**
```prolog
colours([red,yellow]).

predicate(Colours3) :-
    colours(Colours1),
    findall001(Colours1,Colours2),
    findall002(Colours2,Colours3).

% Inner transformation: Y1 = c-Y
findall001([],[]).
findall001([X1|Xs],[X2|Ys]):-
    X2=c-X1,
    findall001(Xs,Ys).

% Outer transformation: [Y2,Y2]
findall002([],[]).
findall002([X|Xs],[[X,X]|Ys]):-
    findall002(Xs,Ys).
```

## Benefits of This Approach

1. **Explicit Control Flow**: The recursive predicates make the iteration explicit
2. **No Side Effects**: Unlike `findall/3` which uses internal state, these predicates are purely functional
3. **Educational Value**: Shows how `findall` can be implemented using basic recursion
4. **Composability**: Each transformation step is a separate predicate that can be tested independently

## Files

- `translator.pl` - The automatic translator program
- `example_usage.pl` - Examples demonstrating the translator
- `README.md` - This documentation
- `ALGORITHM.md` - Detailed algorithm specification

## Implementation Notes

The transformation preserves the semantics of the original `findall` operations:
- **Order preservation**: Elements are processed in the same order
- **Completeness**: All solutions are collected
- **Variable scoping**: Each transformation level operates on the output of the previous level

The numbered predicates (`findall001`, `findall002`, etc.) are named to clearly indicate the transformation sequence and can be traced for debugging.

## License

See LICENSE file for details.
