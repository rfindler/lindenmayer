#lang racket/base
(provide (all-defined-out))
(require graphics/value-turtles
         pict
         (prefix-in : racket/base))

(define (F turtles variables) (cons (draw 2 (car turtles)) (cdr turtles)))
(define (- turtles variables) (cons (turn (:- (hash-ref variables 'θ 90)) (car turtles)) (cdr turtles)))
(define (+ turtles variables) (cons (turn (hash-ref variables 'θ 90) (car turtles)) (cdr turtles)))
(define (|[| turtles variables) (list* (car turtles) (turtle-state (car turtles)) (cdr turtles)))
(define (|]| turtles variables)
  (cons (restore-turtle-state (car turtles) (cadr turtles))
        (cddr turtles)))

(define (start variables)
  (define w (hash-ref variables 'w 500))
  (define h (hash-ref variables 'h 500))
  (cons (move (/ h -2) (turn 90 (move (/ w 10) (turtles w h)))) '()))
(define (finish turtles variables)
  (scale-to-fit (turtles-pict (car turtles)) 300 300))
