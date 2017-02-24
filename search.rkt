#lang racket

(provide find-all)

(define (kmp-search s w [index 0] [eql eq?] [table (build-table w eql)])
  (let loop ([m index] [i 0])
    (cond
      [(< (+ m i) (vector-length s))
       (cond
         [(eql (vector-ref w i)
               (vector-ref s (+ m i)))
          (if (= i (sub1 (vector-length w)))
              m
              (loop m (add1 i)))]
         [else
          (define ti (vector-ref table i))
          (if (> ti -1)
              (loop (- (+ m i) ti) ti)
              (loop (add1 m) 0))])]
      [else #f])))


(define (build-table w [eql eq?])
  (define len (vector-length w))
  (define table (make-vector (+ 2 len)))
  (vector-set! table 0 -1)
  (vector-set! table 1 0)
  (let loop ([pos 2] [cnd 0])
    (when (< pos len)
      (cond
        [(eql (vector-ref w (sub1 pos))
              (vector-ref w cnd))
         (vector-set! table pos (add1 cnd))
         (loop (add1 pos) (add1 cnd))]
        [(> cnd 0)
         (loop pos (vector-ref table cnd))]
        [else
         (vector-set! table pos 0)
         (loop (add1 pos) cnd)])))
  table)

(define (find-all s w [eql eq?])
  (define table (build-table w eql))
  (let loop ([cur 0]
             [indicies null])
    (define i (kmp-search s w cur eql table))
    (cond
      [i (loop (add1 i) (cons i indicies))]
      [else (reverse indicies)])))


(define (string->vector str)
  (for/vector ([c (in-string str)])
    c))
(define (search-string s w)
  (kmp-search (string->vector s) (string->vector w)))


(module+ test
  (require rackunit)
  
  (check-equal? (search-string "foo" "bar") #f)
  (check-equal? (search-string "foobar" "bar") 3)
  (check-equal? (search-string "foobarbaz" "bar") 3)
  ;; use contracts to guard this case ...
  ;(check-equal? (search-string "foobarbaz" "") #f)
  (check-equal? (search-string "" "foobarbaz") #f))
            

  
  