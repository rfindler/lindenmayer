#lang racket

(provide container run-lindenmayer)

#|
;; an Exp α is either:
;;   - (-> α hash α)
;;   - (listof (Container α))
|#

;; content : Exp α
(struct container (content) #:mutable #:transparent)

(define (run-lindenmayer root non-terminals rules start finish variables)
  (define non-terminals-box (box non-terminals))
  (for ([i (in-range (hash-ref variables 'n 4))])
    (rewrite non-terminals-box rules))
  (render-it root start finish variables))

(define (rewrite non-terminals-box rules)
  (define new-non-terminals
    (for/list ([non-terminal (in-list (unbox non-terminals-box))])
      (container (container-content non-terminal))))
  (for ([rule (in-list rules)]
        [non-terminal (in-list (unbox non-terminals-box))])
    (set-container-content! non-terminal (apply rule new-non-terminals)))
  (set-box! non-terminals-box new-non-terminals))

(define (render-it root start finish variables)
  (define current (start variables))
  (let loop ([ele root])
    (cond
      [(container? ele) (loop (container-content ele))]
      [(list? ele) (for ([ele (in-list ele)]) (loop ele))]
      [(procedure? ele) (set! current (ele current variables))]))
  (finish current variables))

(module+ test
  (require rackunit)
  
  (check-equal?
   (let ()
     (define (A-proc val variables) (cons 'A val))
     (define (B-proc val variables) (cons 'B val))
     (define A (container A-proc))
     (define B (container B-proc))
     (run-lindenmayer (container (list A))
                      (list A B)
                      (list (λ (A B) (list A B))
                            (λ (A B) (list A)))
                      (λ (variables) '())
                      (λ (val variables) (reverse val))
                      (hash 'n 4)))
   '(A B A A B A B A)))