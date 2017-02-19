#lang lindenmayer racket

## axiom ##
X

## rules ##
X -> F-[[X]+X]+F[+FX]-X
F -> FF

## variables ##
n=6
θ=22.5
w=400
h=500

============================================================

(provide (all-defined-out))
(require graphics/value-turtles
         (prefix-in : racket/base))

(define (X turtles variables) turtles)
(define (F turtles variables) (cons (draw 3 (car turtles)) (cdr turtles)))
(define (- turtles variables) (cons (turn (:- (hash-ref variables 'θ)) (car turtles)) (cdr turtles)))
(define (+ turtles variables) (cons (turn (hash-ref variables 'θ) (car turtles)) (cdr turtles)))
(define (|[| turtles variables) (list* (car turtles) (turtle-state (car turtles)) (cdr turtles)))
(define (|]| turtles variables)
  (cons (restore-turtle-state (car turtles) (cadr turtles))
        (cddr turtles)))

(define (start variables)
  (define w (hash-ref variables 'w))
  (define h (hash-ref variables 'h))
  (cons (move (/ h -2) (turn 90 (move (/ w 10) (turtles w h)))) '()))
(define (finish turtles variables) (clean (car turtles)))
