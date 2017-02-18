#lang s-exp "lang.rkt"

(require graphics/value-turtles)

(define θ 22.5)
(define (X turtles) turtles)
(define (F turtles) (cons (draw 3 (car turtles)) (cdr turtles)))
(define (- turtles) (cons (turn (- θ) (car turtles)) (cdr turtles)))
(define (+ turtles) (cons (turn θ (car turtles)) (cdr turtles)))
(define (|[| turtles) (list* (car turtles) (turtle-state (car turtles)) (cdr turtles)))
(define (|]| turtles) (cons (restore-turtle-state (car turtles) (cadr turtles))
                            (cddr turtles)))

(define w 400)
(define h 500)
(define start (cons (move (/ h -2) (turn 90 (move (/ w 10) (turtles w h)))) '()))
(define (finish turtles) (clean (car turtles)))

(l-system
 #:convert-start start 
 #:convert-finish finish
 #:iterations 6
 (X)
 (X -> F - |[| |[| X |]| + X |]| + F |[| + F X |]| - X)
 (F -> F F))
