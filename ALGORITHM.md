# Findall to Predicates Conversion Algorithm

This document provides a detailed specification of the algorithm used to convert nested `findall/3` operations into explicit recursive predicates.

## Algorithm Overview

The conversion algorithm transforms Prolog predicates that use nested `findall/3` calls into equivalent predicates using explicit recursion over lists. The algorithm preserves the semantics while eliminating the use of `findall`.

## Algorithm Steps

### 1. Parse the Nested Findall Structure

Given a predicate with nested findall operations:
```prolog
predicate(Result) :- findall(Template_N, (... findall(Template_1, Goal_1, List_1) ...), Result).
```

Parse from innermost to outermost to identify:
- Each findall level
- The template at each level
- The transformation goal at each level
- Base predicates that generate data

### 2. Extract Base Predicates

From the innermost findall goal, identify base predicates:
```prolog
findall(Y1, (colour(Y), Y1=c-Y), Ys)
```

Base predicate: `colour(Y)`

Collect all instances of the base predicate and convert to a list fact:
```prolog
% From:
colour(red).
colour(blue).

% To:
colours([red, blue]).
```

**Naming convention:** Append 's' to make the predicate name plural.

### 3. Analyze Each Findall Level

For each findall from innermost to outermost:

#### Level 1 (Innermost):
```prolog
findall(Y1, (colour(Y), Y1=c-Y), Ys)
```
- **Input source:** Base list `colours([red, blue])`
- **Template:** `Y1` 
- **Transformation:** `Y1=c-Y` means "prepend c- to each element"
- **Output:** List of transformed elements

#### Level 2 (Outer):
```prolog
findall([Y2,Y2], member(Y2, Ys), YYs)
```
- **Input source:** Output from Level 1
- **Template:** `[Y2, Y2]`
- **Transformation:** `member(Y2, Ys)` means "for each element, create [X,X]"
- **Output:** List of doubled elements

### 4. Generate Recursive Predicates

For each findall level N, generate predicate `findallN`:

**Template:**
```prolog
% Base case - empty list produces empty list
findallN([],[]).

% Recursive case - transform head, recurse on tail
findallN([InputHead|InputTail], [OutputHead|OutputTail]) :-
    <transformation of InputHead to OutputHead>,
    findallN(InputTail, OutputTail).
```

**For Level 1** (Transform X to c-X):
```prolog
findall001([],[]).
findall001([X1|Xs],[X2|Ys]):-
    X2=c-X1,
    findall001(Xs,Ys).
```

**For Level 2** (Duplicate as [X,X]):
```prolog
findall002([],[]).
findall002([X|Xs],[[X,X]|Ys]):-
    findall002(Xs,Ys).
```

### 5. Generate Main Predicate

Chain all transformations in sequence:

```prolog
predicate(FinalResult) :-
    base_list(Var1),           % Get base data
    findall001(Var1, Var2),    % First transformation
    findall002(Var2, Var3),    % Second transformation
    ...
    findallNNN(VarN, FinalResult).  % Final result
```

**Variable naming:** Use `Colours1`, `Colours2`, `Colours3`, etc. to track data flow.

## Transformation Rules

### Rule 1: Direct Assignment
If the goal contains `Var = Expression`:
```prolog
findall(Y1, (colour(Y), Y1=c-Y), Ys)
```
Generate:
```prolog
findallN([X|Xs],[Y|Ys]) :-
    Y = c-X,
    findallN(Xs,Ys).
```

### Rule 2: Member Iteration
If the goal is `member(Var, List)`:
```prolog
findall([Y2,Y2], member(Y2, Ys), YYs)
```
Generate:
```prolog
findallN([X|Xs],[[X,X]|Ys]) :-
    findallN(Xs,Ys).
```

### Rule 3: Arithmetic Transformation
If the goal contains arithmetic:
```prolog
findall(Y, (member(X, List), Y is X * 2), Result)
```
Generate:
```prolog
findallN([X|Xs],[Y|Ys]) :-
    Y is X * 2,
    findallN(Xs,Ys).
```

### Rule 4: Compound Templates
If the template is compound:
```prolog
findall(f(X,g(X)), member(X, List), Result)
```
Generate:
```prolog
findallN([X|Xs],[f(X,g(X))|Ys]) :-
    findallN(Xs,Ys).
```

## Complete Example

### Input:
```prolog
colour(red).
colour(blue).

predicate(YYs):-
    findall([Y2,Y2],
        (findall(Y1,(colour(Y),Y1=c-Y),Ys),
         member(Y2,Ys)),
        YYs).
```

### Step-by-step Conversion:

**Step 1:** Identify base predicate `colour(Y)`, convert to list:
```prolog
colours([red, blue]).
```

**Step 2:** Analyze innermost findall:
```prolog
findall(Y1, (colour(Y), Y1=c-Y), Ys)
```
- Transformation: `Y1 = c-Y` 
- Create `findall001` that prepends `c-`

**Step 3:** Analyze outer findall:
```prolog
findall([Y2,Y2], member(Y2, Ys), YYs)
```
- Transformation: wrap in `[X, X]`
- Create `findall002` that duplicates

**Step 4:** Generate helper predicates:
```prolog
findall001([],[]).
findall001([X1|Xs],[X2|Ys]):-
    X2=c-X1,
    findall001(Xs,Ys).

findall002([],[]).
findall002([X|Xs],[[X,X]|Ys]):-
    findall002(Xs,Ys).
```

**Step 5:** Generate main predicate:
```prolog
predicate(Colours3) :-
    colours(Colours1),
    findall001(Colours1,Colours2),
    findall002(Colours2,Colours3).
```

### Output:
```prolog
?- predicate(A).
A = [[c-red, c-red], [c-blue, c-blue]].
```

## Properties Preserved

1. **Order**: Elements are processed in the same order as the original findall
2. **Completeness**: All solutions are collected
3. **Determinism**: Like findall, succeeds exactly once with all solutions
4. **Variable Binding**: Each level operates on the output of the previous level
5. **Semantics**: The logical meaning is preserved

## Advantages

1. **Explicit**: The iteration and transformation logic is explicit
2. **Debuggable**: Each predicate can be tested independently
3. **Educational**: Shows how findall can be implemented with basic recursion
4. **No Side Effects**: Pure functional approach without assert/retract
5. **Composable**: Predicates can be reused in different combinations

## Limitations

1. **Manual Conversion**: Currently requires manual analysis of the findall structure
2. **Naming Convention**: Relies on consistent naming (findall001, findall002, etc.)
3. **No Optimization**: Does not perform optimizations that findall might

## Future Enhancements

Possible extensions to the algorithm:
1. Automated parser to convert findall AST to predicates
2. Optimization passes to eliminate redundant transformations
3. Support for complex goal patterns (cuts, disjunctions)
4. Code generation tools to automate the conversion
