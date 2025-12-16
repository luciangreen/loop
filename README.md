# loop

Converts Prolog `findall/3` to a loop with predicates.

## Overview

This module provides an alternative implementation to Prolog's built-in `findall/3` predicate using explicit loop predicates with backtracking. The `loop/3` predicate is functionally equivalent to `findall/3` but demonstrates how findall operations can be implemented using failure-driven loops and predicate iteration.

## Installation

Load the module in your Prolog program:

```prolog
:- use_module(loop).
```

## Usage

The main predicate is `loop/3`:

```prolog
loop(+Template, +Goal, -List)
```

- **Template**: The term to collect for each solution
- **Goal**: The goal to prove
- **List**: The resulting list of collected solutions

### Basic Examples

```prolog
% Collect all elements from a list
?- loop(X, member(X, [1,2,3]), List).
List = [1, 2, 3].

% Filter with conditions
?- loop(X, (member(X, [1,2,3,4,5,6]), X mod 2 =:= 0), List).
List = [2, 4, 6].

% Arithmetic transformations
?- loop(Y, (member(X, [1,2,3,4]), Y is X * X), List).
List = [1, 4, 9, 16].

% Cartesian product
?- loop(X-Y, (member(X, [a,b]), member(Y, [1,2])), List).
List = [a-1, a-2, b-1, b-2].
```

## Implementation Details

The `loop/3` predicate uses:
1. A failure-driven loop to iterate through all solutions via backtracking
2. Dynamic predicates to temporarily store solutions
3. A unique key (gensym) to isolate different collections
4. Retrieval predicates to collect stored solutions into a list

This demonstrates how `findall/3`-style collection can be implemented using explicit loop constructs rather than relying on the built-in predicate.

## Testing

Run the test suite:

```bash
swipl -g run_all_tests -t halt test_loop.pl
```

Run the examples:

```bash
swipl -g run_all_examples -t halt examples.pl
```

## Files

- `loop.pl` - Main module with `loop/3` implementation
- `test_loop.pl` - Test suite
- `examples.pl` - Usage examples
- `README.md` - This file

## License

See LICENSE file for details.
