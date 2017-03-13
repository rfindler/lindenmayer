#lang racket/base
(require (for-syntax syntax/parse
                     syntax/id-table
                     racket/dict
                     racket/base)
         "runtime.rkt"
         racket/match)
(provide l-system)

(define-syntax (l-system stx)
  (syntax-parse stx
    [(_ start finish iterations (C ...) (A -> B ...) ...)
     (no-duplicates (syntax->list #'(A ...)))
     (with-syntax ([(T ...) (find-terminals #'(B ... ... C ...) #'(A ...))])
       #'(let ()
           (let ([A (cell A)] ...
                 [T (cell T)] ...)
             (register-non-terminals A ...)
             (finish
              (run-lindenmayer iterations
                               (cell (list C ...))
                               (get-non-terminals)
                               (list (rule B ...) ...)
                               start)))))]))

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
    [(_ rhs ...)
     (define ids (for/list ([i (in-range (hash-count non-terminals))])
                   (syntax-local-introduce (hash-ref non-terminals i))))
     #`(λ (lst) (match lst [(list #,@ids) (list rhs ...)]))]))

(define-syntax (get-non-terminals stx)
  #`(list #,@(for/list ([i (in-range (hash-count non-terminals))])
               (hash-ref non-terminals i))))

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

(module+ test
  (require rackunit)

  ;; test case for the algae system
  (let ()
    (define (A so-far) (cons 'A so-far))
    (define (B so-far) (cons 'B so-far))
    (check-equal? (l-system '() reverse 4 (A) (A -> A B) (B -> A))
                  '(A B A A B A B A)))


  ;; test case to make sure duplicate check is correct
  (check-exn
   (λ (x) (and (exn:fail:syntax? x)
               (regexp-match #rx"expected only one rule for.* for A" (exn-message x))))
   (λ ()
     (expand #'(l-system start finish 4 (A) (A -> A B) (A -> A))))))
