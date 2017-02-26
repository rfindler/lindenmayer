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

------------------------------------------------------------

## axiom ##

S L F(p)

## rules ##

S -> [+++G] [---H] T S

G -> +H [-G] L
H -> -G [+H] L

T -> UL
U -> V
V -> TL

L -> [-F(p)] [+F(p)] F(q)

## variables ##

p=1
q=7
n=12
θ=18

============================================================
;; The first four examples are from ABOP p.25
;;
;; The last example is modified from Przemyslaw Prusinkiewicz,
;; Graphical applications of L-systems, Proceedings of Graphics
;; Interface '86, Fig. 3(f)

(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) (all-defined-out))
(define (X turtles variables) turtles)
(define (S turtles variables) turtles)
(define (R turtles variables) turtles)
(define (G turtles variables) turtles)
(define (H turtles variables) turtles)
(define (T turtles variables) turtles)
(define (U turtles variables) turtles)
(define (V turtles variables) turtles)
(define (L turtles variables) turtles)

