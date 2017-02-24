#lang racket/base
(require
  (for-syntax
   syntax/parse
   syntax/id-table
   racket/dict
   racket/base)
  racket/match
  "runtime.rkt")
(provide l-system parametric-l-system default-callbacks)

(define (default-callbacks name args)
  (display name)
  (cond
    [(pair? args)
     (display "(")
     (print (car args))
     (unless (null? (cdr args))
       (for ([arg (in-list (cdr args))])
         (display ",")
         (print arg)))
     (display ")")]))
       

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
     (no-duplicates (syntax->list #'(A ...)))
     (with-syntax ([(T ...) (find-terminals #'(B ... ...) #'(A ...))])
       #'(let ([A (container (add-prefix A))] ...
               [T (container (add-prefix T))] ...)
           (register-non-terminals no A ...)
           (run-lindenmayer (container (list C ...))
                            (get-non-terminals no)
                            (list (rule no B ...) ...)
                            start finish variables)))]))

(define-syntax (parametric-l-system stx)
  (syntax-parse stx
    #:datum-literals (->)
    [(_ no start finish variables
        ((C:id C-args ...) ...)
        ((A:id A-args:id ...) -> (B:id B-args ...) ...) ...)
     (no-duplicates (syntax->list #'(A ...)))
     (with-syntax ([((T T-args ...) ...)
                    (find-terminals/parametric #'((B B-args ...) ... ...)
                                               #'(A ...))])
       (with-syntax ([((A/T A/T-args ...) ...) #'((A A-args ...) ... (T T-args ...) ...)])
         #'(let ()
             (define variable-x variables)
             (struct A (A-args ...) #:transparent) ...
             (struct T (T-args ...) #:transparent) ...
             (define (parametric-l-system-collect val sofar)
               (match val
                 [(A/T A/T-args ...) ((add-prefix A/T) sofar variable-x A/T-args ...)] ...))
             (define axiom (list (box (C C-args ...)) ...))
             (define (parametric-l-system-rewrite boxed-sym)
               (match (unbox boxed-sym)
                 [(A A-args ...) (set-box! boxed-sym (list (box (B B-args ...)) ...))] ...
                 [(T T-args ...) (void)] ...))
             (register-non-terminals no A ...)
             (run-parametric-lindenmayer
              axiom
              parametric-l-system-rewrite
              parametric-l-system-collect
              start finish variable-x))))]))

(define-for-syntax (no-duplicates ids)
  (define table (make-free-id-table))
  (for ([id (in-list ids)])
    (free-id-table-set! table id (cons id (free-id-table-ref table id '()))))
  (for ([(k v) (in-dict table)])
    (unless (= 1 (length v))
      (raise-syntax-error
       'lindenmayer
       (format "expected only one rule for each non-terminal, found ~a for ~a"
               (length v)
               (syntax-e (car v)))
       #f
       (car ids)
       (cdr ids)))))

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

(define-for-syntax (find-terminals/parametric candidates non-terminals)
  (define table (make-free-id-table))
  (for ([non-terminal (in-list (syntax->list non-terminals))])
    (free-id-table-set! table non-terminal #t))
  (define result (make-free-id-table))
  (for ([candidate (in-list (syntax->list candidates))])
    (syntax-case candidate ()
      [(x arg ...)
       (with-syntax ([(arg-x ...) (generate-temporaries #'(arg ...))])
         (unless (free-id-table-ref table #'x #f)
           (free-id-table-set! result #'x #'(x arg-x ...))))]))
  (for/list ([(id exp) (in-dict result)])
    exp))

(module+ test
  (require rackunit)

  (check-equal?
   (let ()
     (define (start variables) '())
     (define (finish lst variables) lst)
     (define (:::A lst variables val) (cons val lst))
     (parametric-l-system
      0
      start finish (hash 'n 1)
      ((A 1))
      ((A x) -> (A (+ x 3)) (A (* x 2)))))
   (reverse '(4 2)))

  (check-equal?
   (let ()
     (define (start variables) '())
     (define (finish lst variables) lst)
     (define (:::A lst variables val) (cons val lst))
     (parametric-l-system
      0
      start finish (hash 'n 2)
      ((A 1))
      ((A x) -> (A (+ x 3)) (A (* x 2)))))
   (reverse '(7 8 5 4)))

  (check-equal?
   (let ()
     (define (start variables) '())
     (define (finish lst variables) lst)
     (define (:::A lst variables val) (cons val lst))
     (parametric-l-system
      0
      start finish (hash 'n 3)
      ((A 1))
      ((A x) -> (A (+ x 3)) (A (* x 2)))))
   (reverse '(10 14 11 16 8 10 7 8))))
