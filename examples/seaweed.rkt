#lang lindenmayer racket
## axiom ##
abF
## rules ##
a → FFf
b → c
c → Fddde[+++++m][-----m]ddfc
d → e
e → g
g → h
h → i
i → j
j → k
k → FF
m → nofF
n → fFF
o → p
p → fF[------A][++++++H]Fq
q → ff[-----B][+++++I]Fr
r → fF[----C][++++J]Fs
s → fF[---D][+++K]Ft
t → fF[--E][++L]F
A → BF
B → Ff+C
C → Ff+D
D → Ff+E
E → Ff+G
G → Ff+
H → IF
I → Ff-J
J → Ff-I
K → Ff-L
L → FF-M
M → Ff-
## variables ##
θ=10
n=11
w=300
h=300
-----------------------
## axiom ##
abF
## rules ##
a → Ff
b → c
c → FFFQ[++++A][----I]FFfd
d → +FFFQ[++++A][----I]FFfe
e → FFFQ[++++A][----I]FFfg
g → -FFFQ[++++A][----I]FFfh
h → FFFQ[++++A][----I]FFfi
i → FFFQ[+++fp]FFfj
j → FFFQ[++++A][----I]FFfk
k → +FFQ[++++A][----I]FFfl
l → FFFQ[++++A][----I]FFfm
m → -FFFQ[++++A][----I]FFfn
n → FFFQ[++++A][----I]FFfo
o → FFFQ[---fp]FFfc
p → abF
A → BCfFFF
B → fF
C → D
D → fFFFE
E → fFF[-P]FG
G → fFF[-P]FH
H → fFF[-P]F
I → JKfFFF
J → fF
K → L
L → fFFFM
M → fFF[+P]FN
N → fFF[+P]FO
O → fFF[+P]F
P → fFfF
Q → FQ
## variables ##
θ=20
n=24
w=300
h=300
=================
#|
From:
COMPUTER SIMULATION OF THE MORPHOLOGY AND
DEVELOPMENT OF SEVERAL SPECIES OF SEAWEED
USING LINDENMAYER SYSTEMS 
|#
(require lindenmayer/turtle)
(provide (all-from-out lindenmayer/turtle) (all-defined-out))
(define (a state . v) state)
(define (b state . v) state)
(define (c state . v) state)
(define (d state . v) state)
(define (e state . v) state)
(define (g state . v) state)
(define (h state . v) state)
(define (i state . v) state)
(define (j state . v) state)
(define (k state . v) state)
(define (l state . v) state)
(define (m state . v) state)
(define (n state . v) state)
(define (o state . v) state)
(define (p state . v) state)
(define (q state . v) state)
(define (r state . v) state)
(define (s state . v) state)
(define (t state . v) state)
(define (A state . v) state)
(define (B state . v) state)
(define (C state . v) state)
(define (D state . v) state)
(define (E state . v) state)
(define (G state . v) state)
(define (H state . v) state)
(define (I state . v) state)
(define (J state . v) state)
(define (K state . v) state)
(define (L state . v) state)
(define (M state . v) state)
(define (N state . v) state)
(define (O state . v) state)
(define (P state . v) state)
(define (Q state . v) state)
