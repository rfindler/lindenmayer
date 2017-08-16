#lang info
(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names (list "L-System Refactor"))

(define deps '("base"
               "data-lib"
               "drracket-plugin-lib"
               "gui-lib"
               ["htdp-lib" #:version "1.5"]
               "parser-tools-lib"
               "pict-lib"
               "pict3d"
               "syntax-color-lib"
               "typed-racket-lib"
               "math-lib"))
(define build-deps '("2d-lib"
                     "rackunit-lib"
                     "pict-doc"
                     "racket-doc"
                     "scribble-lib"
                     "htdp-doc"
                     "syntax-color-doc"
                     "typed-racket-doc"
                     "typed-racket-more"))

(define scribblings '(("scribblings/lindenmayer.scrbl" (multi-page))))

(define pkg-desc "A Lindenmayer system DSL")

(define version "1.1")
