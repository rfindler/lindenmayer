# Experimenting with Racket as a language workbench

This directory contains a simplified implementation of the lindenmayer
language, intended as a starting point to experiment with adding
a feature to a Racket-based programming language.

The feature to add is [stochastic
rules](https://en.wikipedia.org/wiki/L-system#Stochastic_grammars)
(except we will use a slightly different concrete syntax to avoid
conflicting with the syntax used in the full-feature Lindenmayer
language). Here's the notation we'll be using:

```
#lang lindenmayer/simple

## axiom ##

A

## rules ##

A 2 -> AB
A 1 ->
B -> A
```

This means that, with probability 2/3, the `A` will behave by
rewriting into `AB` and with probability 1/3, it will disappear. The
probabilities are obtained by summing all of the weights for a given
non-terminal and then each taking the corresponding share.

Before starting in on changing the code, read the documentation
(search for `lindenmayer/simple` in the docs) to get a sense of how
these pieces interact and have a look at the code to see how they are
implemented.

Probably the best thing is to leave `run-lindenmayer` alone, but to
cause [`lindenmayer-system`](compile.rkt) to synthesize new procedures
that have the random choice embedded into them. So, the first step
will be to add support to `lindenmayer-system` so that the input
syntax accepts weights, e.g.:

```
(lindenmayer-system '()
                      reverse
                      3
                      (A)
                      (A 2 -> A B)
                      (A 1 -> )
                      (B -> A))
```

Adjusting `lindenmayer-system` has two parts: first change the
checking done in `no-duplicates` so that it allows duplicates (when
there are weights specified) and collects rules that have the same
non-terminal together. Next, change the `rule` macro so that it
accepts the weights and the multiple right-hand sides and expands into
a procedure that calls `random` to determine which rule to fire.

Be sure to add some unit tests to the bottom of
[compile.rkt](compile.rkt), to make sure this is implemented
properly. (To test programs with random output, just check some
property of them, e.g., that they do not crash or that the result
contains only `A`s and `B`s.)

Finally, we need to extend the [parser](parse.rkt), which means first
adding a field to capture the result of parsing a rule with a weight
to the declaration in [structs.rkt](structs.rkt), and then extending
the `parse-rule` function to check for a weight and, if one is there,
to record it.
