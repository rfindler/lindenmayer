#lang racket/base
(require (for-syntax racket/base syntax/parse))
(provide define/arity)

(define-syntax (define/arity stx)
  (syntax-parse stx 
    [(_ (f formal:id ...) e)
     #`(begin
         (define (f-proc formal ...) e)
         (define-syntax f
           (λ (stx)
             (syntax-parse stx
               [(the-f actual (... ...))
                (define actual-args-length (length (syntax->list #'(actual (... ...)))))
                (define formal-args-length #,(length (syntax->list #'(formal ...))))
                (unless (= actual-args-length formal-args-length)
                  (raise-syntax-error 'f
                                      (format
                                       "expected ~a arguments for ~a, found ~a"
                                       formal-args-length
                                       'f
                                       actual-args-length)
                                      #f
                                      #'the-f
                                      (list #'f)))
                #'(f-proc actual (... ...))]))))]))

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
