#lang racket/base
(require syntax/readerr
         racket/match)

(current-read-interaction
 (λ (src in)
   ;; TODO: refactor so that the parsing of the `## variables ##`
   ;; section and this code share parsing support
   ;; (also: add better error checking and come up with a way
   ;; to specify things other than the first lindenmayer system)

   (define-values (line-before col-before pos-before) (port-next-location in))
   (define line (read-line in))
   (define-values (line-after col-after pos-after) (port-next-location in))
   (define (failed why)
     (raise-read-error why src line-before col-before pos-before
                       (- pos-after pos-before)))
   
   (cond
     [(eof-object? line) line]
     [else
      (define key-values '())
      (for ([variable-pair (in-list (regexp-split #rx" *; *" line))])
        (define l (regexp-split #rx" *= *" variable-pair))
        (match l
          [`(,lhs ,rhs) (set! key-values (cons `(,(string->symbol lhs)
                                                 . 
                                                 ,(read (open-input-string rhs)))
                                                key-values))]
          [_ (failed "expected variable=value")]))
      (datum->syntax #f
                     `(l-system0 (make-hash '(,@key-values)))
                     (vector src
                             line-before
                             col-before
                             pos-before
                             (- pos-after pos-before)))])))
