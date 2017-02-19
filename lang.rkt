#lang racket/base
(require
  (for-syntax
   syntax/parse
   syntax/id-table
   racket/dict
   racket/base))
(provide l-system)

#|
;; an exp is either:
;;   - symbol?
;;   - (listof container)
|#

;; content : exp
(struct container (content) #:mutable #:transparent)

(define-for-syntax non-terminals (make-hash))

(define-syntax (register-non-terminals stx)
  (syntax-parse stx
    [(_ name ...)
     (for ([name (in-list (syntax->list #'(name ...)))])
       (hash-set! non-terminals (hash-count non-terminals)
                  (syntax-local-introduce name)))
     #'(void)]))
(define-syntax (rule stx)
  (syntax-parse stx
    [(_ lhs rhs ...)
     (define ids (for/list ([i (in-range (hash-count non-terminals))])
                   (syntax-local-introduce (hash-ref non-terminals i))))
     #`(λ (lhs)
         (define to-mutate lhs)
         (λ (#,@ids)
           (set-container-content! to-mutate (list rhs ...))))]))
(define-syntax (axiom stx)
  (syntax-parse stx
    [(_ root a ...)
     #'(define root (container (list a ...)))]))

(define-syntax (get-non-terminals stx)
  #`(list #,@(for/list ([i (in-range (hash-count non-terminals))])
               (hash-ref non-terminals i))))

(define-syntax (l-system stx)
  (syntax-parse stx
    [(_ start finish variables (C ...) (A -> B ...) ...)
     (with-syntax ([(T ...) (find-terminals #'(B ... ...) #'(A ...))])
       #'(let ([A (container (add-prefix A))] ...
               [T (container (add-prefix T))] ...)
           (register-non-terminals A ...)
           (define rules (list (rule A B ...) ...))
           (axiom root C ...)
           (define non-terminals-box (box (get-non-terminals)))
           (run-it root rules non-terminals-box
                   start finish variables)))]))

(define-syntax (add-prefix stx)
  (syntax-parse stx
    [(_ id)
     (datum->syntax #'id
                    (string->symbol (format ":::~a" (symbol->string (syntax-e #'id))))
                    #'id
                    #'id)]))

(define-for-syntax (find-terminals candidates non-terminals)
  (define table (make-free-id-table))
  (for ([non-terminal (in-list (syntax->list non-terminals))])
    (free-id-table-set! table non-terminal #t))
  (define result (make-free-id-table))
  (for ([candidate (in-list (syntax->list candidates))])
    (unless (free-id-table-ref table candidate #f)
      (free-id-table-set! result candidate #t)))
  (for/list ([(id _) (in-dict result)])
    id))

(define (rewrite rules non-terminals-box)
  (define new-non-terminals
    (for/list ([non-terminal (in-list (unbox non-terminals-box))])
      (container (container-content non-terminal))))
  (for ([rule (in-list rules)]
        [non-terminal (in-list (unbox non-terminals-box))])
    (apply (rule non-terminal) new-non-terminals))
  (set-box! non-terminals-box new-non-terminals))

(define (render-it root start finish variables)
  (define current (start variables))
  (let loop ([ele root])
    (cond
      [(container? ele) (loop (container-content ele))]
      [(list? ele) (for ([ele (in-list ele)]) (loop ele))]
      [(procedure? ele) (set! current (ele current variables))]))
  (finish current variables))

(define (run-it root rules non-terminals-box start finish variables)
  (for ([i (in-range (hash-ref variables 'n 4))])
    (rewrite rules non-terminals-box))
  (printf "~a\n" (render-it root start finish variables)))
