#lang scribble/doc

@(require (for-label lindenmayer/3d-turtle pict3d racket/contract
                     (only-in racket vector))
         pict racket/runtime-path scribble/manual scribble/base)

@title{3D L-system turtles}

@defmodule[lindenmayer/3d-turtle]

@racketmodname[lindenmayer/3d-turtle] provides following symbols for L-system interpretation:


@defidentifier[#'F] : Move the turtle forward, and draw a line. If passed an argument it will move that distance,
otherwise it will move 1 unit. If within a set of @racket[\{ \}], record a point in the polygon.

@defidentifier[#'f] : Like @racket[F] but does not draw a line.

@defidentifier[#'G] : Like @racket[F] but does not record a point in a polygon.

@defidentifier[#'\.] : Record a point in the polygon if within @racket[\{ \}].

@defidentifier[#'+] : Yaw the turtle clockwise (Rotate around its ``up'' axis). If a parameter is given
  it rotates by that many degrees, otherwise it uses the variable @racket[δ].

@defidentifier[#'-] : Like @racket[+] but rotates counterclockwise

@defidentifier[#'&] :  Pitch the turtle clockwise (Rotate around its ``left'' axis). If a parameter is given
  it rotates by that many degrees, otherwise it uses the variable @racket[δ].

@defidentifier[#'^] and @defidentifier[#'∧] : Like @racket[&] but counterclockwise.

@defidentifier[#'\\] : Roll the turtle clockwise (Rotate around its ``forward'' axis). If a parameter is given
  it rotates by that many degrees, otherwise it uses the variable @racket[δ].

@defidentifier[#'/] : Like @racket[\\], but counterclockwise.

@defidentifier[#'$] : Roll the turtle so that its ``left'' vector is in the X/Y plain.

@defidentifier[#'!] : Change the current line width. If a parameter is given it is set to that,
   otherwise, the line width is decreased by 40%.

@defidentifier[#'\[] : Save the current turtle state.

@defidentifier[#'\]] : Restore the turtle to the state of the most recent @racket[\[].

@defidentifier[#'\{] : Begin recording vertices for a polygon.

@defidentifier[#'\}] : End recording a polygon. The points recorded between this and corresponding
@racket[\{] are interpreted (and therefore draw), in order, as the boundary of a convex surface. The
surface does not need to be planar. The color of the polygon is controlled by a linear interpolation
of the color of each vertex.

@defidentifier[#'\'] and @defidentifier[#'’] : Increment the position in the color vector, modulo its length. (See
@racket[draw]). The colors controls the line color and the colors of the vertices of polygons.

The library also provides the following functions for creating @racket[start] and @racket[finish] functions,
or building new symbols for the L-system:

@defproc[(turtle-state? [it any/c]) boolean?]{
   Determines if @racket[it] is a turtle state.
}

@defproc[(make-turtle [start dir?] [forward dir?] [up dir?] [line-width positive? 1/4])
         turtle-state?]{
   Construct a turtle who starts a @racket[start], is facing @racket[forward], and who's up is
   @racket[up], with an initial line width of @racket[line-width].

   @racket[up] and @racket[forward] must be unit vectors that are orthogonal to each other.
}

@defthing[starting-turtle turtle-state?]{

  A turtle at (0,0,0), facing up the z axis, with the x axis up, and a line width of @racket[1/4]
}

@defproc[(draw [turtle turtle-state?] [color-vec (vector/c rgba?) (vector (rgba "white" 0))])
         pict3d?]{

  Draw the current turtle to a pict3d. The @racket[color-vec] controls how @racket[\'] and @racket[’]
  are interpreted, with the initial position in the vector being 0.

}

@defproc[(insert-pict [turtle turtle-state?] [pict pict3d?]) turtle-state?]{
  Inserts the given pict at the turtles current location and orientation.
}


@defproc[(set-rendering-config! [width positive?] [height positive?]
                                [#:ambiance? ambiance any/c #f]
                                [#:background background rgba? (rgba "white" 0)]
                                [#:emit emit emitted? default-emitted])
         void?]{

Wraps many of the rendering parameters in @racketmodname[pict3d] for ease of use. @racket[width] and
@racket[height] control the width and height of the pict3d. @racket[ambiance] controls whether or
not there is ambient lighting. @racket[background] sets the background color. @racket[emit] controls
the emitted parameter of all @racket[pict3d?]s rendered.

}
