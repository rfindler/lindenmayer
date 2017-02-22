#lang racket
(provide (all-defined-out))
(require pict3d)
(module+ test (require rackunit))

(define (unit-dir? d)
  (equal? d (dir-normalize d)))
(define (orotho/c v)
  (lambda (k)
    (0.001 . > . (abs (dir-dot v k)))))
;; poor mans non-gimble locking position+rotation
(struct turtle (pos dir up)
  #:transparent
  ;#:extra-constructor-name make-turtle
  )
(define/contract (make-turtle pos dir up)
  (->i ([pos dir?]
        [dir unit-dir?]
        [up (dir) (and/c unit-dir? (orotho/c dir))])
       [result turtle?])
  (turtle pos dir up))
;; pos : Pos, di Dir, up:Dir
;; up and dir *must* be orthoginal
;; up and dir must be unit vectors
(define pipe
  (move
   (cylinder origin
             (pos 1/2 1/2 1))
   (dir -1/4 -1/4 0)))

(define (draw turtles)
  (for/fold ([p empty-pict3d])
            ([start (in-list turtles)]
             [end (in-list (rest turtles))])
    (combine p (turtles->pipe start end))))

;;;; helpers
(define (turtles->pipe start-t end-t)
  (match-define (turtle pos dir _) start-t)
  (match-define (turtle _ _ _) end-t)
  (define-values (θ φ) (dir->angles dir))
  (transform
   pipe
   (affine-compose
    (move pos)
    (linear-compose
     (rotate-z θ)
     (rotate-x φ)))))

;; rotate the turtle left/right by φ degrees
(define (yaw t φ)
  (match-define (turtle pos dir up) t)
  (define dir* (rotate dir up φ))
  (make-turtle pos dir* up))

;; pitch the turtle up or down φ degrees
(define (pitch t φ)
  (match-define (turtle pos dir up) t)
  (define axis (dir-cross dir up))
  (define dir* (rotate dir axis φ))
  (define up* (dir-cross dir* axis))
  (make-turtle pos dir* up*))

;; pitch the turtle by φ degrees
(define (roll t φ)
  (match-define (turtle pos dir up) t)
  (define up* (rotate up dir φ))
  (make-turtle pos dir up*))

;; Dir Dir Degrees -> Dir
;; rotate v1 around v2 by θ
;; v and k must be orthogonal
;; v and k must be unit vectors
;; rotation is clockwise
(define (rotate v k φ)
  (define θ (degrees->radians φ))
  ;; using Rodrigues' rotation formula
  ;; https://en.wikipedia.org/wiki/Rodrigues'_rotation_formula
  (dir-normalize
   (dir+
    (dir-scale v (cos θ))
    (dir+
     (dir-scale (dir-cross k v) (sin θ))
     (dir-scale k (* (dir-dot k v) (- 1 (cos θ))))))))

(module+ test
  (check-equal?
   (rotate (dir 1 0 0)
           (dir 0 0 1)
           90)
   (dir 0 1 0))
  (check-equal?
   (rotate (dir 1 0 0) (dir 0 1 0) 90)
   (dir 0 0 1)))
