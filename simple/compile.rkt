#lang racket/base
(require (for-syntax syntax/parse
                     syntax/id-table
                     racket/dict
                     racket/base)
         "run.rkt"
         racket/match)
(provide lindenmayer-system)

(define-syntax (lindenmayer-system stx)
  (syntax-parse stx
    [(_ start finish iterations (C:id ...) (A:id -> B:id ...) ...)
     (no-duplicate-ids (syntax->list #'(A ...)))
     (with-syntax ([(S ...) (remove-duplicate-ids #'(A ... B ... ... C ...))])
       #'(let ([S (cell S)] ...)
           (finish
            (run-lindenmayer iterations
                             (cell (list C ...))
                             (list A ...)
                             (list (rule (A ...) (B ...)) ...)
                             start))))]))

(define-syntax (rule stx)
  (syntax-parse stx
    [(_ (non-terminal ...) (symbol ...))
     #`(λ (lst)
         (match lst
           [(list non-terminal ...)
            (list symbol ...)]))]))

(define-for-syntax (no-duplicate-ids ids)
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

(define-for-syntax (remove-duplicate-ids symbols)
  (define table (make-free-id-table))
  (for ([symbol-id (in-list (syntax->list symbols))])
    (free-id-table-set! table symbol-id #t))
  (for/list ([(symbol-id _) (in-dict table)])
    symbol-id))

(module+ test
  (require rackunit)

  ;; test cases for the `rule` macro; checking
  ;; that it builds an appropriate function
  (let ()
    (define a-rule (rule (A B C) (A C A)))
    (check-equal? (a-rule (list 'a 'b 'c))
                  (list 'a 'c 'a))
    (check-equal? (a-rule (list 1 2 3))
                  (list 1 3 1)))
  
  ;; test case for the algae system
  (let ()
    (define (A so-far) (cons 'A so-far))
    (define (B so-far) (cons 'B so-far))
    (check-equal? (lindenmayer-system '() reverse 4 (A) (A -> A B) (B -> A))
                  '(A B A A B A B A)))


  ;; test case to make sure duplicate check is correct
  (check-exn
   (λ (x) (and (exn:fail:syntax? x)
               (regexp-match? #rx"expected only one rule for.* for A" (exn-message x))))
   (λ ()
     (expand #'(lindenmayer-system start finish 4 (A) (A -> A B) (A -> A))))))
