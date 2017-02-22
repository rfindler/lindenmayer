#lang racket
(provide (all-defined-out))
(require pict3d (prefix-in 3d: pict3d))
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


(define line-size 1/4)
(define ls/2 (/ line-size 2))
(define cap (sphere origin ls/2))
(define line-length 1)
(define pipe
  (freeze
   (combine
    cap
    (move cap (dir line-length 0 0)) 
    (rotate-y
     (move
      (cylinder origin
                (pos line-size line-size line-length))
      (dir (- ls/2) (- ls/2) 0))
     90))))

(define (draw turtles)
  (for/fold ([p empty-pict3d])
            ([start (in-list turtles)]
             [end (in-list (rest turtles))])
    (combine p (turtles->pipe start end))))

;;;; helpers
(define (turtles->pipe start-t end-t)
  (match-define (turtle start _  _) start-t)
  (match-define (turtle end _ _) end-t)
  (define dir (dir- end start))
  (define-values (yaw pitch) (dir->angles dir))
  (define crossdir (dir-cross +x dir))
  (define axis (dir-cross +x dir))
  (define angle
    (radians->degrees
     (acos
      (/ (dir-dot +x dir)
         (* (dir-dist +x) (dir-dist dir))))))
  (transform
   pipe
   (affine-compose
    (move start)
    (if (zero-dir? axis)
        (if (= 180.0 angle) (3d:rotate +z angle) identity-affine)
        (3d:rotate axis angle)))))

(define (zero-dir? axis)
  (and (zero-ish? (dir-dx axis))
       (zero-ish? (dir-dy axis))
       (zero-ish? (dir-dz axis))))

(define (zero-ish? x)
  (define threshold 0.0000001)
  (> threshold x (- threshold)))

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
  (define up* (rotate up axis φ))
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


