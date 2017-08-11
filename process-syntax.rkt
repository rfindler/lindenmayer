#lang racket

(provide build-refactor-info handle-expansion)

(require syntax/parse
         "syntax-properties.rkt")

;; Need to ensure non-false values are put in the refactor-table
(struct wrap (prop) #:prefab)

(define (get-refactor-property stx)
  (define maybe-prop (syntax-property stx rule-property-key))
  (process-property maybe-prop))

(define (process-property prop)
  (cond
    [(rule-group? prop) (list (rule-group-info prop))]
    [(cons? prop)
     (match-define (cons left right) prop)
     (append (process-property left)
             (process-property right))]
    [else '()]))

;; traverse fully expanded syntax and produce a list suitable for building
;; an interval-map.
(define (build-refactor-info stx source)
  (define found (mutable-set))
  (define refactor-info '())
  (let loop ([stx stx])
    (define props (get-refactor-property stx))
    (for ([prop (in-list props)])
      (when (not (set-member? found prop))
        (set! refactor-info (cons prop refactor-info))
        (set-add! found prop)))
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
