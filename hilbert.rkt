#lang lindenmayer racket
## axiom ##
A

## rules ##
A → B - F + C F C + F - D & F ∧ D - F + & & C F C + F + B / /
B → A & F ∧ C F B ∧ F ∧ D ∧ ∧ - F - D ∧ | F ∧ B | F C ∧ F ∧ A / /
C → | D ∧ | F ∧ B - F + C ∧ F ∧ A & & F A & F ∧ C + F + B ∧ F ∧ D / /
D → | C F B - F + B | F A & F ∧ A & & F B - F + B | F C / /

## variables ##
n=2
δ=90
w=500
h=250

============================================================

(provide (all-defined-out))
(require "3d-turtle.rkt"
         (prefix-in r: racket)
         (only-in pict3d dir-scale dir+ zero-dir +x +z))

;; turn left
(define (+ state variables)
  (define δ (hash-ref variables 'δ))
  (cons (yaw (first state) δ) state))
;; turn right
(define (- state variables)
  (define δ (hash-ref variables 'δ))
  (cons (yaw (first state) (r:- δ)) state))
;; pitch down
(define (& state variables)
  (define δ (hash-ref variables 'δ))
  (cons (pitch (first state) δ) state))
;; pitch up
(define (∧ state variables) (void)
  (define δ (hash-ref variables 'δ))
  (cons (pitch (first state) (r:- δ)) state))
;; roll right
(define (/ state variables)
  (define δ (hash-ref variables 'δ))
  (cons (roll (first state) δ) state))
(define (\\ state variables)
  (define δ (hash-ref variables 'δ))
  (cons (roll (first state) (r:- δ)) state))
;; reverse
(define (\| state variables)
  (match-define (turtle pos dir up) (first state))
  (cons (yaw (first state) 180) state))

;; move
(define (F state variables)
  (match-define (turtle pos dir up) (first state))
  (define pos* (dir+ pos dir))
  (cons (make-turtle pos* dir up) state))

(define (A state variables) state)
(define (B state variables) state)
(define (C state variables) state)
(define (D state variables) state)


(define (start variables)
  (list (turtle zero-dir +x +z)))
(define (finish turtles variables)
  (draw turtles))
