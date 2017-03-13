#lang racket/base

(require "../parse.rkt" "../../lexer.rkt")

(provide
 (rename-out
  [-read read]
  [-read-syntax read-syntax]
  [-get-info get-info]))

(define (-read port) (syntax->datum (-read-syntax #f port #f #f #f #f)))
(define (-read-syntax name port source line col position) (parse-module port name))

(define (-get-info port source line col position)
  (λ (key default)
    (case key
      [(color-lexer) (wrap-lexer #f)]
      [else default])))
