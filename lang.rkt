#lang racket/base
(require
  (for-syntax
   syntax/parse
   syntax/id-table
   racket/dict
   racket/base)
  "runtime.rkt")
(provide l-system)

(define-for-syntax all-non-terminals (make-hash))

(define-syntax (register-non-terminals stx)
  (syntax-parse stx
    [(_ no name ...)
     (define non-terminals (make-hash))
     (for ([name (in-list (syntax->list #'(name ...)))])
       (hash-set! non-terminals (hash-count non-terminals)
                  (syntax-local-introduce name)))
     (hash-set! all-non-terminals (syntax-e #'no) non-terminals)
     #'(void)]))

(define-syntax (rule stx)
  (syntax-parse stx
    [(_ no rhs ...)
     (define non-terminals (hash-ref all-non-terminals (syntax-e #'no)))
     (define ids (for/list ([i (in-range (hash-count non-terminals))])
                   (syntax-local-introduce (hash-ref non-terminals i))))
     #`(λ (#,@ids) (list rhs ...))]))

(define-syntax (get-non-terminals stx)
  (syntax-parse stx
    [(_ no)
     (define non-terminals (hash-ref all-non-terminals (syntax-e #'no)))
     #`(list #,@(for/list ([i (in-range (hash-count non-terminals))])
                  (hash-ref non-terminals i)))]))

(define-syntax (l-system stx)
  (syntax-parse stx
    [(_ no start finish variables (C ...) (A -> B ...) ...)
     (with-syntax ([(T ...) (find-terminals #'(B ... ...) #'(A ...))])
       #'(let ([A (container (add-prefix A))] ...
               [T (container (add-prefix T))] ...)
           (register-non-terminals no A ...)
           (run-lindenmayer (container (list C ...))
                            (get-non-terminals no)
                            (list (rule no B ...) ...)
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
