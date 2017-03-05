#lang scribble/doc

@(require pict racket/runtime-path scribble/manual scribble/base "util.rkt" racket/list
          (for-label racket/contract
                     (only-in racket/base hash?  define require provide
                              all-from-out all-defined-out except-in hash-ref vector)
                     pict3d
                     lindenmayer/3d-turtle))

@title{@tt{#lang lindenmayer}, a language for L-Systems}

@defmodulelang[lindenmayer]

The Lindenmayer language provides a language for running and interpreting Lindenmayer Systems.

In general L-systems are useful for modeling plant growth, procedural content generation, and making
pretty pictures:

@(define-runtime-path flowery-bush.png "../screenshots/flowery-bush.png")
@(bitmap-draft-mode #f)
@(scale (bitmap flowery-bush.png) 0.4)

@table-of-contents[]

@section{A Quick Introduction to L-Systems}
An L-System is a string rewriting system, where at each step every non-terminal is rewritten. Each
system has one @tt{axiom}, which is the starting string, a set of @tt{rules} that describe how to
rewrite, and some @tt{variables} that can control the rewriting and interpreting of the system. For
example, the system:

@codeblock{
#lang lindenmayer
## axiom ##
A
## rules ##
A -> AB
B -> A
## variables ##
n=3
}

Starts with the string @tt{A}, and at each step replaces every @racket[A] with @racket[AB], and
every @racket[B] with an @racket[A]. After three iterations (which is controlled by the variable
@racket[n]), the string will be @racket[ABAAB].


@section{Interpreting L-systems}

Just rewriting strings isn't very interesting. L-Systems become useful when the resulting strings
are interpreted. For example, the following L-System is interpreted as a turtle graphics program,
where @racket[F], means draw, @racket[-] and @racket[+] mean rotate by @racket[θ] degrees, and
@racket[\[] and @racket[\]] mean save and restore the turtles state:

@codeblock{
#lang lindenmayer racket

## axiom ##
X

## rules ##
X -> F[+X]F[-X]+X
F -> FF

## variables ##
n=7
θ=20

=============================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) X)
(define (X turtles variables) turtles)
}

And running this system produces a picture of a tree branch:

@(define-runtime-path abop-25-d.rkt "../examples/abop-page25-d.rkt")
@(first (fetch-picts abop-25-d.rkt))

In general @tt{#lang lindenmayer} can be extended by a section language on the @tt{#lang} line, in
this case @racket[racket]. Then below the line of @racketcommentfont{=====} there should be a module written in
that language that tells the l-system how it should be interpreted. The program must export one
function matching each terminal and non-terminal of the l-system, plus the function @racket[start]
and @racket[finish]. In our example the @racketmodname[lindenmayer/turtle] library provides most of these
functions.

The L-system is interpret by first creating an initial state using the @racket[start] function.
Then the function matching each character of the string are called on the state and the variables to
produce a new state. Finally the @racket[finish] function is called to produce the final result.

The @racket[start] function should match the contract @racket[(-> hash? any/c)]. It takes a hash
table mapping each variable to its value. The functions for each character of the string should
match the contract @racket[(-> any/c hash? any/c)]. They take in the current state and variable
mapping and produce a new state. The @racket[finish] function should match the contract
@racket[(-> any/c hash? any/c)]. It takes in the current state and variables and produces the final
product of the system.

The @tt{lindenmayer} package comes with the @racketmodname[lindenmayer/turtle] and
@racketmodname[lindenmayer/3d-turtle] collections for L-system interpretation


@section{Parameteric L-systems}

Non-terminals in L-systems can carry values that maybe updated at each step. These parameter lists
are surrounded with parentheses and their elements are separated by commas, much like the
mathematical notation for function application. Each parameter is written in a separate arithmetic
language meaning, e.g., that the context determines if a @racket[+] is rotating the turtle (when it
appears outside of a parameter list) or addition (when it appears inside a parameter list).

Example:
@codeblock{
#lang lindenmayer racket
## axiom ##
A(1,0.1)
## rules ##
A(l,w) → !(w)F(l)[&(a)B(l*s,w*y)]/(d)A(l*r,w*y)
B(l,w) → !(w)F(l)[-(b)$C(l*s,w*y)]C(l*r,w*y)
C(l,w) → !(w)F(l)[+(b)$B(l*s,w*y)]B(l*r,w*y)
## variables ##
n=10
r=0.9
s=0.6
a=45
b=45
d=136.75
y=0.707

w=2000
h=2000

dist=3.5
rot=0
shift=-3.2

========================================
(provide (all-defined-out)
         (all-from-out lindenmayer/3d-turtle))
(require lindenmayer/3d-turtle
         (except-in pict3d move))

(define (A state variables l w) state)
(define (B state variables l w) state)
(define (C state variables l w) state)
(define (start variables)
  (make-turtle (dir 0 0 (hash-ref variables 'shift)) +z +x))


(define (finish turtles variables)
  (define dist (hash-ref variables 'dist))
  (define v (angles->dir (hash-ref variables 'rot) 0))
  (define camera
    (basis 'camera
           (point-at (pos+ origin (dir-scale v dist))
                     (pos 0 0 .01)
                     #:up +x)))
  (set-rendering-config!
   (hash-ref variables 'w)
   (hash-ref variables 'h)
   #:ambiance? #f)
  (combine
   camera
   (draw turtles (vector (rgba "brown") (rgba "saddlebrown") (rgba "chocolate")))))
}

Draws a black-and-white tree:

@(define-runtime-path btq.rkt "../screenshots/better-trees1.png")
@(scale (bitmap btq.rkt) 0.4)

In this example @racket[A], @racket[B], and @racket[C] carry two parameters, @racket[l] and
@racket[w] (which are the length and width of the line the 3d-turtle will draw).

When a Symbol has extra parameters those parameters are passed as extra arguments to the definitions
below the @racketcommentfont{=========}.

@section{Conditional L-systems}

@(define-runtime-path alternating.rkt "../examples/leaf-alternating.rkt")

An L-system with parameters can dispatch on those parameters, picking between rewrites for a single
symbol. For example:

@codeblock{
#lang lindenmayer racket

## axiom ##
A(0)
## rules ##
A(d) : d > 0 -> A(d-1)
A(d) : d = 0 -> F(1/2)[+A(D)]F(1/2)B(0)
B(d) : d > 0 -> B(d-1)
B(d) : d = 0 -> F(1/2)[-B(D)]F(1/2)A(0)
F(a)         -> F(a*R)
## variables ##
D=1
R=1.36
n=20
θ=45
============================================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) (all-defined-out))
(define (A turtles variables . _) turtles)
(define (B turtles variables . _) turtles)
}

Has the @racket[A] and @racket[B] rules dispatch on the parameter @racket[d], to produce:

@(first (fetch-picts alternating.rkt))


Conditionals are placed before @racketidfont["->"], and after a @racketidfont[":"]. Conditions can be joined by an
@racket[&]. Currently supported comparisons are: @racket[≠ =< > ≤ ≥].



@include-section["turtles.scrbl"]
@include-section["3d-turtle.scrbl"]
