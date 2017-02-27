#lang info
(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names (list "L-System Refactor"))

(define deps '("base"
               "data-lib"
               "drracket-plugin-lib"
               "gui-lib"
               "htdp-lib"
               "parser-tools-lib"
               "pict-lib"
               "pict3d"
               "syntax-color-lib"
               "typed-racket-lib"))
(define build-deps '("2d-lib"
                     "rackunit-lib"))

(define pkg-desc "A Lindenmayer system DSL")
