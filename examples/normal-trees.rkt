#lang lindenmayer racket

## axiom ##
!(r) F(l) A(r)

## rules ##
A(r) -> ^F(l)  B(r+s)
        >(δ*3) B(r+s)
        >(δ*5) B(r+s)
B(w) -> [!(w) ^^F(l) >(δ*6) A(w)]

## variables ##
n=13
δ=18

r=0.5
s=-0.079

l=1.8

w=800
h=800
---------------------------

## axiom ##
!(r) F(l) A(r,l)

## rules ##
A(r,l) -> ^F(l)  B(r,l)
          >(θ*3) B(r,l)
          >(θ*4) B(r,l)
B(r,l) -> [!(r) ^F(l) >(θ*2) A(r*s,l*v)]

## variables ##
n=14
θ=26
δ=20

r=0.4
s=0.65

l=1.8
v=0.93

w=800
h=800
===========================
;; modified from http://www.geekyblogger.com/2008/04/tree-and-l-system.html

(provide (all-defined-out)
         (all-from-out lindenmayer/3d-turtle))
(require lindenmayer/3d-turtle
         (except-in pict3d move)
         (prefix-in pict: pict))

(define (A state vars . _) state)
(define (B state vars . _) state)
(define > /)

(define (start variables)
  (make-turtle (dir 0 -15 0) +y +z))

(define opos (dir 0 -0.5 18))
(define odir (dir-normalize opos))
(define odist (* 1.15 (dir-dist opos)))

(define camera (point-at (pos+ origin (dir-scale odir odist)) (pos 0 0 0)))

(define (finish turtles variables)
  (pict:scale
   (draw-pict turtles camera)
   12))
