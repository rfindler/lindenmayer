#lang racket

(provide build-refactor-info handle-expansion)

(require syntax/parse
         "syntax-properties.rkt")

;; Need to ensure non-false values are put in the refactor-table
(struct wrap (prop) #:prefab)

(define (get-refactor-property stx)
  (syntax-property stx rule-property-key))

;; traverse fully expanded syntax and produce a list suitable for building
;; an interval-map.
(define (build-refactor-info stx source)
  (define refactor-info '())
  (let loop ([stx stx])
    (define prop (get-refactor-property stx))
    (when prop
      (set! refactor-info (cons prop refactor-info)))
    (when (syntax->list stx)
      (for ([sub-stx (in-syntax stx)])
        (loop sub-stx))))
  (build-pre-interval-map (reverse refactor-info)))

(define (build-pre-interval-map table)
  (sort (apply
         append
         (for/list ([rule-group (in-list table)]
                    #:when (and rule-group (car rule-group)))
           (match-define (cons ranges refactor-info) rule-group)
           (match-define (cons start (list rule-ranges ...)) ranges)
           (for/list ([range (in-list rule-ranges)]
                      #:when range)
             (cons range (cons start refactor-info)))))
        (match-lambda**
         [((cons start1 end1) (cons start2 end2))
          (or (< start1 start2)
              (and (= start1 start2)
                   (>= end1 end2)))])
        #:key car))

(define (handle-expansion stx path source cust)
  (and (syntax? stx)
       (build-refactor-info stx source)))
