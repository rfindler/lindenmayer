#lang racket
(provide + - & ∧ ^ \\ / \| \[ \] ’ \' ! F f G
         $ \{ \} \.
         ;; from 3d-turtle.rkt
         insert-pict make-turtle starting-turtle draw set-rendering-config! turtle-state? draw-pict)
(require "3d.rkt" (prefix-in r: racket))
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
(define (! state variables [set #f])
  (if set
      (set-width state set)
      (grow state -0.4)))
(define (F state variables [distance 1])
  (move state distance))
(define (G state variables [distance 1])
  (move/no-poly state distance))
(define (f state variables [distance 1])
  (shift state distance))
(define (\. state variables)
  (save-vertex state))
(define ($ state variables)
  (reorient-to-up state))
(define (\{ state variables)
  (start-poly state))
(define (\} state variables)
  (end-poly state))
