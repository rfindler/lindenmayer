#lang racket
(require (for-syntax syntax/parse))
(provide define/arity)

(define-syntax (define/arity stx)
  (syntax-parse stx 
    [(_ (def-site-f formal:id ...) e)
     #`(begin
         (define-syntax (def-site-f stx)
           (syntax-parse stx
             [(use-site-f actual (... ...))
              (define actual-args-length
                (syntax-length #'(actual (... ...))))
              (define formal-args-length
                #,(syntax-length #'(formal ...)))
              (unless (= actual-args-length formal-args-length)
                (signal-length-error formal-args-length
                                     actual-args-length
                                     #'def-site-f #'use-site-f))
              #'(f-proc actual (... ...))]))
         (define (f-proc formal ...) e))]))
;; STOP

(define-for-syntax (signal-length-error formal-args-length actual-args-length def-site-f use-site-f)
  (define f (syntax-e def-site-f))
  (raise-syntax-error f
                      (format
                       "expected ~a arguments for ~a, found ~a"
                       formal-args-length
                       f
                       actual-args-length)
                      #f
                      use-site-f
                      (list def-site-f)))

(define-for-syntax (syntax-length stx) (length (syntax->list stx)))

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
