#lang lindenmayer racket

## axiom ##
F

## rules ##
F -> F[+F]F[-F]F

## variables ##
n=5
θ=25.7
w=250
h=500

------------------------------------------------------------

## axiom ##
F

## rules ##
F -> F[+F]F[-F][F]

## variables ##
n=5
θ=20
w=150
h=150

------------------------------------------------------------

## axiom ##
F

## rules ##
F -> FF-[-F+F+F]+[+F-F-F]

## variables ##
n=4
θ=22.5
w=150
h=150

------------------------------------------------------------

## axiom ##
X

## rules ##
X -> F[+X]F[-X]+X
F -> FF

## variables ##
n=7
θ=20
w=400
h=550

------------------------------------------------------------

## axiom ##
X

## rules ##
X -> F[+X][-X]FX
F -> FF

## variables ##
n=7
θ=25.7
w=350
h=550

------------------------------------------------------------

## axiom ##
X

## rules ##
X -> F-[[X]+X]+F[+FX]-X
F -> FF

## variables ##
n=6
θ=22.5
w=350
h=350

============================================================
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) (all-defined-out))
(define (X turtles variables) turtles)

