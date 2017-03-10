#lang scribble/doc

@(require (for-label lindenmayer/turtle (only-in pict pict?))
          pict racket/runtime-path scribble/manual scribble/base
          "deflsymbol.rkt")


@title{2D L-system turtles}

@defmodule[lindenmayer/turtle]


@racketmodname[lindenmayer/turtle] provides support for 2D
turtle graphics in L-systems.

@deflsymbol[(F () (len _ ...))
            ([len real?])]{
 Moves the turtle forward and draws a line.

 If @racket[len] is supplied, the turtles moves that distance (negative values move backwards).
 Otherwise, it moves two steps forward.
}


@deflsymbol[(f () (len _ ...))
            ([len real?])]{
 Like @racket[F], but does not draw a line.
}

@deflsymbol[(- () (_ ...)) ()]{
 Rotates the turtle counterclockwise. It rotates by @racket[θ] degrees if that variable is
 defined, and by @racket[90] degrees otherwise.
}

@deflsymbol[(+ () (_ ...)) ()]{
 Like @racket[-], but rotates clockwise.
}

@deflsymbol[(|/| () (width _ ...))
            ([width (and/c real? positive?)])]{
 Set the turtle line drawing with. The default @racket[width] is @racket[1].
}

@deflsymbol[(|[| () (_ ...)) ()]{
 Save the current turtle state.
}

@deflsymbol[(|]| () (_ ...)) ()]{
  Restore the turtle to the state of the matching @racket[\[].
}

@defproc[(start [variables hash?]) turtles?]{
 Creates an initial, with the turtle facing
 to the right.
}

@defproc[(finish [variables hash?] [turtles turtles?]) pict?]{
  Turns the given @racket[turtles] into a pict.
}
