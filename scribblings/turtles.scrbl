#lang scribble/doc

@(require (for-label lindenmayer/turtle (only-in pict pict?))
         pict racket/runtime-path scribble/manual scribble/base)


@title{2D L-system turtles}

@defmodule[lindenmayer/turtle]

@racketmodname[lindenmayer/turtle] provides support for 2D turtle graphics in L-systems. It provides the
following symbols for L-system interpretation:

@defidentifier[#'F] : Move the turtle forward, and draw a line. If passed an argument it will move that distance,
otherwise it will move 2 units.

@defidentifier[#'f] : like @racket[F], but does not draw a line.

@defidentifier[#'-] : rotate the turtle counterclockwise. It rotates by @racket[θ] degrees if that variable is
defined, and by @racket[90] degrees otherwise.

@defidentifier[#'+] : like @racket[-], but rotates clockwise.

@defidentifier[#'!] : Set the turtle line drawing with. If provided a parameter it sets the turtles with to that,
otherwise it sets it to 1.

@defidentifier[#'\[] : Save the current turtle state.

@defidentifier[#'\]] : Restore the turtle to the state of the matching @racket[\[].

In addition this library provides an @defidentifier[#'start] function to create an initial turtle facing up,
and an @defidentifier[#'finish] function that turns that turtle into a @racket[300] by @racket[300]
@racket[pict?].
