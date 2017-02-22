#lang racket

(provide (contract-out (struct rule ([nt identifier?] [rhs (listof identifier?)]))))

(struct rule (nt rhs) #:transparent)