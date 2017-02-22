#lang racket

(provide annotate-rule-info)

(define rule-property-key 'lindemayer:rule-info)

(define (annotate-rule-info rules stx)
  (syntax-property
   stx
   rule-property-key
   (process-rules rules)))

(define (process-rules rules)
  (void))
