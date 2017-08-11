#lang racket
(require (for-syntax syntax/parse))
(provide define/arity)

(define-syntax (define/arity stx)
  (syntax-parse stx 
    [(_ (def-f formal:id ...) e)
     #`(begin
         (define-syntax (def-f stx)
           (syntax-parse stx
             [(use-f formal ...)
              #'(f-proc formal ...)]
             [(use-f actual (... ...))
              (signal-length-error
               #'(actual (... ...))
               #'(formal ...)
               #'def-f #'use-f)]))
         (define (f-proc formal ...)
           e))]))
;; STOP

(define-for-syntax (signal-length-error actual-args formal-args def-f use-f)
  (define formal-args-length (stx-len formal-args))
  (define actual-args-length (stx-len actual-args))
  (define f (syntax-e def-f))
  (raise-syntax-error f
                      (format
                       (string-append
                        "definition has ~a argument~a, use has ~a argument~a,"
                        " expected them to be the same")
                       formal-args-length
                       (if (= formal-args-length 1) "" "s")
                       actual-args-length
                       (if (= actual-args-length 1) "" "s"))
                      #f
                      use-f
                      (list def-f)))

(define-for-syntax (stx-len stx) (length (syntax->list stx)))

(module+ test
  (require rackunit)
  (check-equal?
   (let ()
     (define/arity (f x y z) (+ x y z))
     (f 1 2 3))
   6)
  (check-exn
   exn:fail:syntax?
   (λ ()
     (expand #'(let ()
                 (define/arity (f) 1)
                 (f 2)))))
  (check-exn
   exn:fail:syntax?
   (λ ()
     (expand #'(let ()
                 (define/arity (f x y z w) 1)
                 (f 1 2))))))
