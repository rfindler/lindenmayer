#lang racket

(provide
 (contract-out
  (struct rule ([nt sym?] [rhs (listof sym?)]))
  (struct sym ([id identifier?] [args (listof any/c)]))))
(struct sym (id args) #:transparent)
(struct rule (nt rhs) #:transparent)
