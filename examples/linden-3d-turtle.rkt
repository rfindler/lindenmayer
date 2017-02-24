#lang racket
(provide + - & ∧ ^ \\ / \| \[ \] ’ \' ! F
         ;; from 3d-turtle.rkt
         insert-pict make-turtle starting-turtle draw set-rendering-config!)
(require "3d-turtle.rkt" (prefix-in r: racket))
;; turn left
(define (+ state variables [δ (hash-ref variables 'δ)])
  (yaw state δ))
;; turn right
(define (- state variables [δ (hash-ref variables 'δ)])
  (yaw state (r:- δ)))
;; pitch down
(define (& state variables [δ (hash-ref variables 'δ)])
  (pitch state δ))
;; pitch up
(define (∧ state variables [δ (hash-ref variables 'δ)])
  (pitch state (r:- δ)))
(define ^ ∧)
;; roll right
(define (\\ state variables [δ (hash-ref variables 'δ)])
  (roll state δ))
(define (/ state variables [δ (hash-ref variables 'δ)])
  (roll state (r:- δ)))
(define (\| state variables)
  (yaw state 180))
(define (\[ state variables)
  (save state))
(define (\] state variables)
  (restore state))
(define (’ state variables)
  (shift-color-index state 1))
(define \' ’)
(define (! state variables [shift -0.4])
  (grow state shift))
(define (F state variables [distance 1])
  (move state distance))

