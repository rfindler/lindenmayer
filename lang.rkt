#lang racket
(require (for-syntax syntax/parse))
(provide (rename-out [module-begin #%module-begin]))


#|
;; an exp is either:
;;   - symbol?
;;   - (listof container)
|#

;; content : exp
(struct container (content) #:mutable #:transparent)

(define-for-syntax non-terminals (make-hash))

(define-syntax (non-terminal stx)
  (syntax-parse stx
    [(_ name)
     (hash-set! non-terminals (hash-count non-terminals)
                (syntax-local-introduce #'name))
     #'(define name (container 'name))]))
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

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ (C ...) (A -> B ...) ...)
     #'(#%plain-module-begin
         (non-terminal A) ...
         (define rules (list (rule A B ...) ...))
         (axiom root C ...)
         (define non-terminals-box (box (get-non-terminals)))
         (run-it root rules non-terminals-box))]))

(define (rewrite rules non-terminals-box)
  (define new-non-terminals
    (for/list ([non-terminal (in-list (unbox non-terminals-box))])
      (container (container-content non-terminal))))
  (for ([rule (in-list rules)]
        [non-terminal (in-list (unbox non-terminals-box))])
    (apply (rule non-terminal) new-non-terminals))
  (set-box! non-terminals-box new-non-terminals))

(define (to-str ele)
  (apply
   string-append
   (flatten
    (let loop ([ele ele])
      (cond
        [(container? ele) (loop (container-content ele))]
        [(list? ele) (map loop ele)]
        [(symbol? ele) (symbol->string ele)])))))

(define (size obj)
  (define ht (make-hasheq))
  (let loop ([obj obj])
    (cond
      [(hash-ref ht obj #f) 0]
      [else
       (hash-set! ht obj #t)
       (match obj
         [(container content) (+ (loop content) 1)]
         [(? list? l) (apply + 1 (map loop obj))]
         [else 1])])))

(define (run-it root rules non-terminals-box)
  (for/list ([i (in-range 8)])
    (rewrite rules non-terminals-box)
    (printf "~a ~a\n" (size root) (to-str root))))
