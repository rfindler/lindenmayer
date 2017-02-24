#lang racket

(provide
 (contract-out
  (struct rule ([nt sym?] [guard (or/c #f syntax?)] [rhs (listof sym?)]))
  (struct sym ([id identifier?] [args (listof any/c)]))))
(struct sym (id args) #:transparent)
(struct rule (nt guard rhs) #:transparent)
