#lang typed/racket
(provide run-lindenmayer cell)

(struct (α) cell ([item : (Lindenmayer-Dag α)]) #:mutable)

(define-type (Lindenmayer-Dag α)
  (U (-> α α)
     (Listof (cell α))))

(: run-lindenmayer
   (All (α)
        (-> Natural
            (cell α)
            (Listof (cell α))
            (Listof (-> (Listof (cell α))
                        (Listof (cell α))))
            α
            α)))
(define (run-lindenmayer iterations axiom nts rules init)
  (for/fold ([nts nts])
            ([i (in-range iterations)])
    (: new-nts (Listof (cell α)))
    (define new-nts
      (for/list ([nt (in-list nts)])
        (cell (cell-item nt))))
    (for ([nt (in-list nts)]
          [rule (in-list rules)])
      (set-cell-item! nt (rule new-nts)))
    new-nts)
  (collect axiom init))

(: collect (All (α) (-> (cell α) α α)))
(define (collect axiom init)
  (define current init)
  (let loop ([ele (cell-item axiom)])
    (cond
      [(list? ele)
       (for ([ele (in-list ele)])
         (loop (cell-item ele)))]
      [else
       (set! current (ele current))]))
  current)

(module+ test
  (require typed/rackunit)
  
  (check-equal?
   (let ()
     (: A-proc (-> (Listof Symbol) (Listof Symbol)))
     (define (A-proc val) (cons 'A val))
     (: B-proc (-> (Listof Symbol) (Listof Symbol)))
     (define (B-proc val) (cons 'B val))
     (define A (cell A-proc))
     (define B (cell B-proc))
     (reverse
      ((inst run-lindenmayer (Listof Symbol))
       4
       (cell (list A))
       (list A B)
       (list (λ (lst) (list (list-ref lst 0) (list-ref lst 1)))
             (λ (lst) (list (list-ref lst 0))))
       '())))
   '(A B A A B A B A)))
