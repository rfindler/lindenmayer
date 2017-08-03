#lang racket/base
(require
  (for-syntax
   syntax/parse
   syntax/id-table
   racket/dict
   racket/base
   racket/list)
  racket/set
  racket/match
  "runtime.rkt"
  "private/define-arity.rkt")
(provide l-system parametric-l-system default-callbacks
         join-hashes)

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
     (with-syntax ([(T ...) (find-terminals #'(B ... ... C ...) #'(A ...))])
       #'(let ([A (container (add-prefix A))] ...
               [T (container (add-prefix T))] ...)
           (register-non-terminals no A ...)
           (run-lindenmayer (container (list C ...))
                            (get-non-terminals no)
                            (list (rule no B ...) ...)
                            start finish variables)))]))

(define-syntax (parametric-l-system stx)

  (define-splicing-syntax-class maybe-guard
    (pattern (~seq G:expr) #:with no-guard? #f)
    (pattern (~seq) #:with G #'#t #:with no-guard? #t))
  
  (syntax-parse stx
    #:datum-literals (->)
    [(_ no start finish variables
        ((C:id C-args ...) ...)
        ((A:id A-args:id ...) G:maybe-guard -> (B:id B-args ...) ...) ...)
     (no-duplicates (syntax->list #'(A ...))
                    (map syntax-e (syntax->list #'(G.no-guard? ...))))
     ;; nd- stands for "no-duplicates-"
     (with-syntax ([((nd-A (nd-A-args ...) A-body) ...)
                    (combine-duplicates-into-body-expressions
                     #'(((A A-args ...) G.G -> (box (B (arith-expression B-args) ...)) ...) ...))])
       (with-syntax ([((T T-args ...) ...)
                      (find-terminals/parametric #'((B B-args ...) ... ... (C C-args ...) ...)
                                                 #'(A ...))])
         (with-syntax ([((A/T A/T-args ...) ...)
                        #'((nd-A nd-A-args ...) ... (T T-args ...) ...)]
                       [(A-make ...) (generate-temporaries #'(nd-A ...))]
                       [(T-make ...) (generate-temporaries #'(T ...))])
           (with-syntax ([arity-checkers
                          #'(begin
                              (define/arity (nd-A nd-A-args ...) (A-make nd-A-args ...)) ...
                              (define/arity (T T-args ...) (T-make T-args ...)) ...)])
             #'(let ()
                 (define variable-x variables)
                 (struct nd-A (nd-A-args ...) #:transparent #:extra-constructor-name A-make) ...
                 (struct T (T-args ...) #:transparent #:extra-constructor-name T-make) ...
                 (define (parametric-l-system-collect val sofar)
                   (match val
                     [(A/T A/T-args ...) ((add-prefix A/T) sofar variable-x A/T-args ...)] ...))
                 (define axiom
                   (let ()
                     arity-checkers
                     (list (box (C (arith-expression C-args) ...)) ...)))
                 (define (parametric-l-system-rewrite boxed-sym)
                   (match (unbox boxed-sym)
                     [(nd-A nd-A-args ...)
                      arity-checkers
                      (set-box! boxed-sym A-body)] ...
                     [(T T-args ...) (void)] ...))
                 (register-non-terminals no nd-A ...)
                 (run-parametric-lindenmayer
                  axiom
                  parametric-l-system-rewrite
                  parametric-l-system-collect
                  start finish variable-x))))))]))

(define-syntax (arith-expression stx)
  (syntax-parse stx
    [(_ e)
     (let loop ([e #'e])
       (syntax-parse e
         [(op:id e ...)
          (with-syntax ([op (datum->syntax #'here (syntax-e #'op) #'op #'op)])
            #`(op #,@(for/list ([e (in-list (syntax->list #'(e ...)))])
                       (loop e))))]
         [id:identifier #'id]
         [n:number #'n]))]))
          

(define-for-syntax (combine-duplicates-into-body-expressions stx)
  (syntax-parse stx
    [(((A:id A-args:id ...) G:expr -> . expr) ...)
     (define table (make-free-id-table))
     (for ([A (in-list (syntax->list #'(A ...)))]
           [A-args (in-list (syntax->list #'((A-args ...) ...)))]
           [G (in-list (syntax->list #'(G ...)))]
           [body (in-list (syntax->list #'(expr ...)))])
       (free-id-table-set! table A (cons (list A-args G #`(list . #,body))
                                         (free-id-table-ref table A '())))
       (void))
     (for/list ([(A rev-infos) (in-dict table)])
       (define infos (reverse rev-infos))
       (define last-args (list-ref (car rev-infos) 0))
       (define first-args (list-ref (car infos) 0))
       (define body-expr
         (for/fold ([expr #`(signal-no-match-error
                             '#,A
                             #,@last-args)])
                   ([last-info (in-list (append (cdr rev-infos) (list #f)))]
                    [info (in-list rev-infos)])
           (define A-args (list-ref info 0))
           (define G (list-ref info 1))
           (define body (list-ref info 2))
           (define let-body
             #`(if #,G
                   #,body
                   #,expr))
           (if last-info
               (with-syntax ([(A-arg ...) A-args]
                             [(last-A-arg ...) (list-ref last-info 0)])
                 #`(let ([A-arg last-A-arg] ...)
                     #,let-body))
               let-body)))
       (list A first-args body-expr))]))

(define (signal-no-match-error nt . args)
  (error 'lindenmayer
         "none of the conditions held for symbol ~s with argument~a~a"
         nt
         (if (= 1 (length args)) "" "s")
         (apply
          string-append
          (for/list ([arg (in-list args)])
            (format " ~e" arg)))))

(define-for-syntax (no-duplicates ids [consider-id?s (map (λ (x) #t) ids)])
  (define table (make-free-id-table))
  (for ([id (in-list ids)]
        [consider-id? (in-list consider-id?s)])
    (when consider-id?
      (free-id-table-set! table id (cons id (free-id-table-ref table id '())))))
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

(define (join-hashes hash1 hash2)
  (for/hash ([k (in-set (list->set (append (hash-keys hash1) (hash-keys hash2))))])
    (values k (hash-ref hash2 k (λ () (hash-ref hash1 k))))))

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
   (reverse '(10 14 11 16 8 10 7 8)))


  (check-equal?
   (let ()
     (define (start variables) '())
     (define (finish lst variables) lst)
     (define (:::A lst variables val) (cons `(A ,val) lst))
     (define (:::Q lst variables val) (cons `(Q ,val) lst))
     (parametric-l-system
      0
      start finish (hash 'n 3)
      ((A 2))
      ((A x) (> x 0) -> (Q x) (A (- x 1)))
      ((A y) (= y 0) -> (Q 0))))
   '((Q 0) (Q 1) (Q 2)))

  (check-equal?
   (let ()
     (define (start variables) '())
     (define (finish lst variables) lst)
     (define (:::A lst variables val) (cons `(A ,val) lst))
     (define (:::Q lst variables val) (cons `(Q ,val) lst))
     (parametric-l-system
      0
      start finish (hash 'n 10)
      ((A 5))
      ((A x) (> x 3) -> (Q (* x x x)) (A (- x 1)))
      ((A y) (> y 0) -> (Q y) (A (- y 1)))
      ((A z) (= z 0) -> (Q (+ z -1234)))))
   (reverse (list '(Q 125) '(Q 64) '(Q 3) '(Q 2) '(Q 1) '(Q -1234))))

  (check-equal?
   (let ()
     (define (start variables) '())
     (define (finish lst variables) lst)
     (define (:::A lst variables val) (cons `(A ,val) lst))
     (define (:::- lst variables) (cons '- lst))
     (parametric-l-system
      0
      start finish (hash 'n 3)
      ((A 6))
      ((A x) -> (-) (A (- x 1)))))
   (reverse (list '- '- '- '(A 3)))))
