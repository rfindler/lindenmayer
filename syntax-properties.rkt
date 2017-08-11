#lang racket

(provide annotate-rule-info
         rule-property-key
         (struct-out rule-group))
(require "structs.rkt" syntax/id-table)

(define rule-property-key 'lindemayer:rule-info)

(define (annotate-rule-info rules stx)
  (syntax-property
   stx
   rule-property-key
   (process-rules rules)))

(module bi-maps racket

  (provide
   (contract-out (struct bi-map ([syn->sym mutable-free-id-table?] [sym->syn hash?]))
                 [make-bi-map (-> bi-map?)]
                 [bi-map-set! (->i ([map bi-map?]
                                    [k (or/c symbol? identifier?)]
                                    [v (k) (and/c (or/c symbol? identifier?)
                                                  (if (symbol? k)
                                                      identifier?
                                                      symbol?))])
                                   [result any/c])]
                 [bi-map-add! (->i ([map bi-map?]
                                    [k (or/c symbol? identifier?)]
                                    [v (k) (and/c (or/c symbol? identifier?)
                                                  (if (symbol? k)
                                                      identifier?
                                                      symbol?))])
                                   [result any/c])]
                 [bi-map-ref (->i ([map bi-map?]
                                   [k (or/c symbol? identifier?)])
                                  [result (k) (if (symbol? k)
                                                  identifier?
                                                  symbol?)])]))
  
  (require syntax/id-table)
  
  (struct bi-map (syn->sym sym->syn))
  (define (make-bi-map)
    (bi-map (make-free-id-table) (make-hash)))
  
  (define (bi-map-set! map k v)
    (cond
      [(identifier? k)
       (free-id-table-set! (bi-map-syn->sym map) k v)
       (hash-set! (bi-map-sym->syn map) v k)]
      [else
       (hash-set! (bi-map-sym->syn map) k v)
       (free-id-table-set! (bi-map-syn->sym map) v k)]))

  (define (bi-map-add! map k v)
    (cond
      [(identifier? k)
       (unless (bi-map-ref map k)
         (bi-map-set! map k v))]
      [else
       (unless (bi-map-ref map v)
         (bi-map-set! map k v))]))
  

  (define (bi-map-ref map k [failure #f])
    (cond
      [(identifier? k)
       (free-id-table-ref (bi-map-syn->sym map) k failure)]
      [else
       (hash-ref (bi-map-sym->syn map) k failure)])))
       
(require 'bi-maps)

(struct rule-group (info) #:prefab)

(define (process-rules rules)
  (define symbol-map (make-bi-map))
  (define has-args? #f)
  ;; fill in the mapping from identifiers to symbols
  (for/list ([r (in-list rules)])
    (match-define (rule nt guard rhs) r)
    (match-define (sym nt-id nt-args) nt)
    (unless (empty? nt-args)
      (set! has-args? #t))
    (bi-map-add! symbol-map nt-id (gensym))
    (for ([s (in-list rhs)])
      (match-define (sym id args) s)
      (bi-map-add! symbol-map id (gensym))))
  ;; build a list of vectors of symbols for each rule
  (define rule-strings
    ;(listof (cons/c (vectorof symbol? #:flat? #t) (vectorof (cons/c integer? integer?) #:flat? #t)))
    (for/list ([r (in-list rules)])
      (match-define (rule nt guard rhs) r)
      (cons
       (for/vector ([s (in-list rhs)])
         (match-define (sym id _) s)
         (bi-map-ref symbol-map id))
       (for/vector ([s (in-list rhs)])
         (match-define (sym id _) s)
         (cons (sub1 (syntax-position id)) (syntax-span id))))))
  ;; build a mapping from source positions to symbolic representations of (non-)terminals
  (define position-map
    #;(hash/c integer?
              (cons/c integer? symbol?)
              #:immutable #t
              #:flat? #t)
    (for*/hash ([r (in-list rules)]
                [s (in-list (rule-rhs r))])
      (match-define (sym id _) s)
      (values (sub1 (syntax-position id))
              (cons (syntax-span id) (bi-map-ref symbol-map id)))))
  (define ranges
    ;(or/c #f (cons/c integer? (listof (or/c #f (cons/c integer? integer?)))))
    (and (not (empty? rules))
         (cons (sub1 (syntax-position (sym-id (rule-nt (first rules)))))
               (for/list ([r (in-list rules)])
                 (define rhs (rule-rhs r))
                 (and (not (empty? rhs))
                      (cons
                       ;; start of the rhs
                       (for/first ([s (in-list rhs)])
                         (match-define (sym id _) s)
                         (sub1 (syntax-position id)))
                       ;; end of the rhs
                       (for/last ([s (in-list rhs)])
                         (match-define (sym id _) s)
                         (+ (syntax-position id)
                            (syntax-span id)))))))))
  
  (and (not has-args?) ; disable the refactoring for parametric l-systems
       ranges
       (rule-group (list ranges position-map rule-strings))))
