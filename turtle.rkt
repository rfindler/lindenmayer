#lang racket/base
(provide (all-defined-out))
(require graphics/value-turtles
         pict
         (prefix-in : racket/base))

(define (F turtles variables [length 2] . ignored)
  (cons (draw length (car turtles)) (cdr turtles)))
(define (f turtles variables [length 2] . ignored)
  (cons (move length (car turtles)) (cdr turtles)))
(define (- turtles variables [factor 1] . ignored)
  (cons (turn (:* factor -1 (hash-ref variables 'θ 90)) (car turtles)) (cdr turtles)))
(define (+ turtles variables [factor 1] . ignored)
  (cons (turn (:* factor (hash-ref variables 'θ 90)) (car turtles)) (cdr turtles)))
(define (|[| turtles variables) (list* (car turtles) (turtle-state (car turtles)) (cdr turtles)))
(define (|]| turtles variables)
  (cons (restore-turtle-state (car turtles) (cadr turtles))
        (cddr turtles)))

(define (start variables)
  (define w (hash-ref variables 'w 500))
  (define h (hash-ref variables 'h 500))
  (cons (move (/ h -2) (turn 90 (move (/ w 10) (turtles w h)))) '()))
(define (finish turtles variables)
  (define orig (turtles-pict (car turtles)))
  (define scaled (scale-to-fit orig 300 300))
  (define scale-factor (/ (pict-width scaled) (pict-width orig)))
  ;; when the pict is scaled, lines that are drawn with width 1
  ;; will now be drawn at the width `scale-factor`, so we inset
  ;; the pict by 1/2 of that for the bits of lines that'll be
  ;; hanging off the edge of the pict
  (inset scaled (* 1/2 scale-factor)))
