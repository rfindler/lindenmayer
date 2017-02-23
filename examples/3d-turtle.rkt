#lang racket
(provide
 (rename-out
  [out:make-turtle make-turtle]
  [out:move move])
 yaw
 pitch
 roll
 save
 restore
 draw
 set-rendering-config!)
(require pict3d (prefix-in 3d: pict3d))
;;TODO lazy load gui
(require racket/gui)
(module+ test (require rackunit))

(define (out:make-turtle start dir up)
  (define turtle*
    (make-turtle
     start
     (dir-normalize dir)
     (dir-normalize up)))
  (turtle-state turtle* empty (list start) empty))

(struct turtle-state (turtle stack points points-stack))

(define (yaw t φ)
  (match-define (turtle-state tur s p ps) t)
  (turtle-state (turtle-yaw tur φ) s p ps))
(define (pitch t φ)
  (match-define (turtle-state tur s p ps) t)
  (turtle-state (turtle-pitch tur φ) s p ps))
(define (roll t φ)
  (match-define (turtle-state tur s p ps) t)
  (turtle-state (turtle-roll tur φ) s p ps))
(define (out:move t d)
  (match-define (turtle-state tur s p ps) t)
  (define t* (turtle-move tur d))
  (turtle-state t* s (cons (turtle-pos t*) p) ps))
(define (save t)
  (match-define (turtle-state tur s p ps) t)
  (turtle-state tur (cons tur s) empty (cons p ps)))
(define (restore t)
  (match-define (turtle-state _ (cons tur s) p ps) t)
  (match-define (turtle pos _ _) tur)
  (turtle-state tur s (list pos) (cons p ps)))

(define (draw t)
  (match-define (turtle-state _ _ points points-stack) t)
  (for/fold ([p empty-pict3d])
            ([points (in-list (cons points points-stack))])
    (combine p (draw-points points))))

(define current-texturing
  (make-parameter (list default-color default-emitted)))
(define (set-rendering-config! width height #:ambiance? [ambiance? #f])
  (current-pict3d-background (rgba "white" 0))
  (current-pict3d-add-indicators? #f)
  (cond
    [ambiance?
     (current-texturing (list default-color default-emitted))
     (current-pict3d-ambient (emitted "white" 1))
     (current-pict3d-add-sunlight? #t)]
    [else
     (current-texturing (list (rgba "black") (emitted "black" 0)))
     (current-pict3d-ambient (emitted "black" 0))
     (current-pict3d-add-sunlight? #f)])
  (current-pict3d-width width)
  (current-pict3d-height height))

(define (unit-dir? d)
  (dir=? d (dir-normalize d)))

(define (ortho? v k)
    (0.00000001 . > . (abs (dir-dot v k))))
;; poor mans non-gimble locking position+rotation
(struct turtle (pos dir up)
  #:transparent)
(define/contract (make-turtle pos dir up)
  (->i ([pos dir?]
        [dir unit-dir?]
        [up unit-dir?])
       #:pre (up dir) (ortho? up dir)
       [result turtle?])
  (turtle pos dir up))
;; pos : Pos, di Dir, up:Dir
;; up and dir *must* be orthoginal
;; up and dir must be unit vectors


(define line-size 1/4)
(define ls/2 (/ line-size 2))
(define cap (sphere origin ls/2))
(define line-length 1)
(define pipe-cache (make-hash))
(define (make-pipe)
  (hash-ref!
   pipe-cache
   (current-texturing)
   (lambda ()
     (with-color (first (current-texturing))
       (with-emitted (second (current-texturing))
         (freeze
          (combine
           cap
           (move cap (dir line-length 0 0))
           (rotate-y
            (move
             (cylinder origin
                       (pos line-size line-size line-length))
             (dir (- ls/2) (- ls/2) 0))
            90))))))))

(define (draw-points pts)
  (define pipe (make-pipe))
  (for/fold ([p empty-pict3d])
            ([start (in-list pts)]
             [end (in-list (rest pts))])
    (combine p (dirs->pipe start end pipe))))

;;;; helpers
(define (dirs->pipe start end pipe)
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

(define (dir=? d1 d2)
  (and (=-ish? (dir-dx d1) (dir-dx d2))
       (=-ish? (dir-dy d1) (dir-dy d2))
       (=-ish? (dir-dz d1) (dir-dz d2))))

(define (zero-dir? axis)
  (dir=? axis zero-dir))

(define (zero-ish? x)
  (=-ish? 0 x))

(define (=-ish? x y)
  (define threshold 0.0000001)
  (> threshold (- x y) (- threshold)))

;; rotate the turtle left/right by φ degrees
(define (turtle-yaw t φ)
  (match-define (turtle pos dir up) t)
  (define dir* (dir-normalize (rotate dir up φ)))
  (make-turtle pos dir* up))

;; pitch the turtle up or down φ degrees
(define (turtle-pitch t φ)
  (match-define (turtle pos dir up) t)
  (define axis (dir-cross dir up))
  (define dir* (dir-normalize (rotate dir axis φ)))
  (define up* (dir-normalize (rotate up axis φ)))
  (make-turtle pos dir* up*))

;; pitch the turtle by φ degrees
(define (turtle-roll t φ)
  (match-define (turtle pos dir up) t)
  (define up* (dir-normalize (rotate up dir φ)))
  (make-turtle pos dir up*))

(define (turtle-move t d)
  (match-define (turtle pos dir up) t)
  (define pos* (dir+ pos (dir-scale dir d)))
  (make-turtle pos* dir up))

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
