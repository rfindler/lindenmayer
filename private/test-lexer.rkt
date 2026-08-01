#lang racket/base

(require (for-syntax racket/base)
         racket/runtime-path)
(define-runtime-path abop-25a.rkt (build-path 'up "examples" "abop-page25-a.rkt"))

(module+ test
  (require framework racket/class racket/contract
           (only-in (submod "../main.rkt" reader) get-info)
           "../lexer.rkt")

  ;; run random testing 8 times
  (for ([i (in-range 8)])
    ;; `lexer/c`'s random testing would exercise the lexer
    (new color:text-mode%
         [get-token (wrap-lexer #f)]))
  ;; or get an instance of the lexer from `get-info`?
  ;; should be the same
  (for ([i (in-range 8)])
    (call-with-input-file abop-25a.rkt
      (lambda (inp)
        (port-count-lines! inp)
        (for ([i (in-range 5)]) ; "#lang"
          (read-char inp))
        (new color:text-mode%
             [get-token
              ((get-info inp abop-25a.rkt 1 5 4)
               'color-lexer #f)])))))
