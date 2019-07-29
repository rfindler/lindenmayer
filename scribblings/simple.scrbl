#lang scribble/manual

@(require
   racket/pretty
   lindenmayer/simple/parse
   scribble/example
   (for-label graphics/value-turtles
              lindenmayer/simple/parse
              lindenmayer/simple/run
              lindenmayer/simple/compile
              lindenmayer/simple/lex
              typed/racket/base
              syntax-color/lexer-contract))

@(define lindenmayer-eval
   (let ()
     (define le (make-base-eval))
     (le '(require pict racket/math racket/class racket/draw
                   racket/list pict/balloon pict/flash))
     (le '(require pict
                   graphics/value-turtles
                   lindenmayer/simple/parse
                   lindenmayer/simple/run
                   lindenmayer/simple/compile
                   lindenmayer/simple/lex))
     le))

@title{Simplified Lindenmayer System Language}

@defmodulelang[lindenmayer/simple]

The @tt{lindenmayer/simple} language is a trimmed down
version of @tt{#lang lindenmayer} that supports only a
single Lindenmayer system and does not support parametric or
conditional Lindenmayer systems, nor does it support
interoperability with other languages. It is intended to be
a digestible example of a language implemented with
@tt{#lang}.

Here is one example use of the language.

@(define example @list{
#lang lindenmayer/simple
## axiom ##
A
## rules ##
A -> AB
B -> A
## variables ##
n=3
})

@(apply typeset-code example)

When it is run, it produces the output
@(let ()
   (define ip (open-input-string (apply string-append example)))
   (define op (open-output-string))
   (parameterize ([current-namespace (make-base-namespace)]
                  [current-output-port op]
                  [read-accept-reader #t])
     (eval (read-syntax #f ip))
     (namespace-require ''name))
   (unless (equal? (get-output-string op)
                   "ABAAB\n")
     (error 'lindenmayer/scribblings/simple
            "didn't get the expected output, got ~s"
            (get-output-string op)))
   (tt (regexp-replace #rx"\n" (get-output-string op) ""))).

There are three main pieces to the implementation of the language:
the parser, which translates the notations above into a use
of @racket[lindenmayer-system], the macros that translate that
into a call to the @racket[run-lindenmayer] function, and then
that function itself.

@section{The Parser}

@defmodule[lindenmayer/simple/parse]

@defproc[(parse-module [port input-port?] [name any/c]) syntax?]{
 Parses a Lindenmayer system program from @racket[port], treating
 @racket[name] as the the location of the @racket[port]'s content.

 The resulting syntax object is a @racket[module] form that
 contains a call to @racket[lindenmayer-system]. For example, the result
 of calling @racket[parse-module] on the example above produces a
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax object}
 whose shape (when stripped of all lexical content
 and source location information) is this expression:
 @(let ()
    (define in (open-input-string
                (apply string-append (cdr example))))
    (define out (open-output-string))
    (parameterize ([pretty-print-columns
                    (+ 1 (string-length "  (require lindenmayer/simple/compile)"))])
      (pretty-print (syntax->datum (parse-module in #f)) out))
    (verbatim
     (get-output-string out)))
}

@section{The Compiler}

@defmodule[lindenmayer/simple/compile]

@defform[(lindenmayer-system
          start-expr finish-expr iterations-expr
          axiom
          rule ...)
         #:grammar ([axiom (id ...)]
                    [rule
                     (id -> id ...)
                     (id → id ...)])
         #:contracts ([start-expr any/c]
                      [finish-expr (-> any/c any/c)]
                      [iterations-expr natural?])]{
 Runs the given Lindenmayer system, where the axiom and rules
 are sequences of identifiers that are expected to be bound
 to procedures that accept one argument. Once the Lindenmayer
 system completes, the procedures are called on the resulting
 string, in the order of the identifiers in the string, and
 are used to read a result out of the lindenmayer system.

 This form is implemented by compiling into a call to
 @racket[run-lindenmayer].
 
 This example starts from @tt{A}, iterates
 the Lindenmayer system 3 times to arrive at the string
 @tt{ABAAB}. Then it traverses the string making calls
 to the procedures @racket[A] and @racket[B] in the given
 order. So, first it calls @racket[A] with @racket['()],
 which returns @racket[(cons 'A '())], and so forth. Finally,
 it calls @racket[reverse] on the elements of the list.
 @examples[#:eval lindenmayer-eval #:label #f
           (eval:no-prompt (define (A lst) (cons 'A lst)))
           (eval:no-prompt (define (B lst) (cons 'B lst)))
           (lindenmayer-system '()
                               reverse
                               3
                               (A)
                               (A -> A B)
                               (B -> A))]

 Instead of just pulling the string back out of the
 system, we can use turtle graphics to extract
 a picture:
 @examples[#:label #f #:eval lindenmayer-eval
           (eval:no-prompt (define (X t) t))
           (eval:no-prompt (define (Y t) t))
           (eval:no-prompt (define (F t) (draw 4 t)))
           (eval:no-prompt (define (↰ t) (turn -90 t)))
           (eval:no-prompt (define (↱ t) (turn 90 t)))
           (lindenmayer-system (turn 90 (turtles 100 100))
                               turtles-pict
                               10
                               (F X)
                               (X -> X ↰ Y F ↰)
                               (Y -> ↱ F X ↱ Y))] 
}

@section{The Runtime}

@defmodule[lindenmayer/simple/run]

This module is implemented in Typed Racket, so its inputs
are described as types. (Typed Racket can be called from
Racket, and vice-versa.)

@deftogether[(
@defform[ #:kind "type" (Lindenmayer-Dag α)]{}
@defstruct[cell ([item (Lindenmayer-Dag α)]) #:mutable]{})]{

The @racket[Lindenmayer-Dag] and @racket[cell] types
are the runtime representation of the Lindenmayer system as it
evolves. Roughly, they are trees with lists of children at
each interior node. The leaves are procedures that are used
when the final tree has been constructed to extract the value.
The trees are actually DAGs and the sharing is used to get
better performance while building the final string. Once that
final string is constructed, the extraction process traverses
the DAG, calling each of the procedures in the leaf nodes.

The @racket[(Lindenmayer-Dag α)] type is equivalent to
 @racketblock[(U (-> α α)
                 (Listof (cell α)))]
}

@defproc[(run-lindenmayer [iterations Natural]
                          [axiom (cell α)]
                          [nts (Listof (cell α))]
                          [rules (Listof (-> (Listof (cell α))
                                             (Listof (cell α))))]
                          [init α])
         α]{
 Implements the runtime support for evaluating a Lindenmayer system.

 The @racket[axiom] is expected to be an interior node whose children
 are all leaves (and thus contain procedures). The @racket[nts] and
 the @racket[rules] arguments are expected to have the same length;
 each pair of elements (one from each argument) together represent
 a single rule. When performing a rewriting step, @racket[run-lindenmayer]
 replaces each non-terminal by invoking the corresponding
 element of @racket[rules].

 The @racket[init] argument is used after the final Lindenmayer string
 is constructed. It is passed to the first leaf in the DAG representing
 the Lindenmayer string and then the result of that procedure is passed
 to the next, and so on, until the last one, whose result is the result
 of the entire call to @racket[run-lindenmayer]

 @examples[#:eval lindenmayer-eval
           (eval:no-prompt (define (A-proc val) (cons 'A val)))
           (eval:no-prompt (define (B-proc val) (cons 'B val)))
           (eval:no-prompt (define A (cell A-proc)))
           (eval:no-prompt (define B (cell B-proc)))
           (reverse
            (run-lindenmayer
             4
             (cell (list A)) (code:comment "the axiom, as a cell")
             (list A B)      (code:comment "the non-terminals")

             (code:comment "procedures that perform the rule rewrites")
             (list (λ (lst) (list (list-ref lst 0) (list-ref lst 1)))
                   (λ (lst) (list (list-ref lst 0))))

             (code:comment "the initial value")
             '()))]
 
}

@section{Syntax Colorer}

@defmodule[lindenmayer/simple/lex]

@defthing[lindenmayer-lexer lexer/c]{
 Called to determine the tokenization of programs in @tt{#lang lindenmayer/simple}.
}

@close-eval[lindenmayer-eval]
