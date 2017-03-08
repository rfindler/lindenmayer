#lang racket
(provide fetch-picts)
(require (prefix-in p: pict))
(define fetch-picts-ns (make-base-namespace))
(namespace-attach-module (current-namespace)
                         'pict
                         fetch-picts-ns)
(define fetch-picts-cache (make-hash))
(define (fetch-picts file)
  (define key (~a (simplify-path file)))
  (hash-ref! fetch-picts-cache
             (simplify-path key)
             (λ () (fetch-picts/run-it file))))
(define (fetch-picts/run-it file [show-progress? #f])
  (define picts '())
  (parameterize ([current-namespace fetch-picts-ns]
                 [current-print
                  (λ (val)
                    (when (p:pict? val)
                      (when show-progress? (display #\.) (flush-output))
                      (set! picts (cons val picts))))])
    (dynamic-require file #f))
  (when show-progress? (printf " done\n"))
  (reverse picts))
