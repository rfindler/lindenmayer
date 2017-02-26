#lang racket
(provide
 (rename-out
  [out:make-turtle make-turtle]
  [out:move move])
 shift
 move/no-poly
 yaw
 pitch
 roll
 save
 restore
 grow
 shift-color-index
 draw
 starting-turtle
 insert-pict
 set-width
 save-vertex
 reorient-to-up
 nudge
 rotate-about
 turtle-facing
 start-poly
 end-poly
 set-rendering-config!)
(require pict3d (prefix-in 3d: pict3d))
;;TODO lazy load gui
(require racket/gui)
(module+ test (require rackunit))

(define line-size 1/4)
(define ls/2 (/ line-size 2))
(define line-length 1)

(define (out:make-turtle start dir up [line-size line-size])
  (define turtle*
    (make-turtle
     start
     (dir-normalize dir)
     (dir-normalize up)
     line-size
     0))
  (turtle-state turtle* empty (list (turtle->point turtle*)) empty empty empty))

(struct turtle-state (turtle stack points points-stack points-set-stack extra-picts)
  #:transparent)
#|
(listof (listof points))
|#

(struct polygon (points)
  #:transparent
  #:extra-constructor-name make-polygon)
(struct point (dir width color-index)
  #:transparent
  #:extra-constructor-name make-point)

(struct extras (point dir pict)
  #:transparent
  #:extra-constructor-name make-extras)

(define (turtle-facing t)
  (match-define (turtle-state (turtle pos d u _ c) s p ps polys extras) t)
  d)

(define (rotate-about t axis α)
  (match-define (turtle-state tur s p ps poly es) t)
  (turtle-state (turtle-rotate tur axis α) s p ps es))

(define (nudge t H T)
  (match-define (turtle-state tur s p ps poly extras) t)
  (turtle-state (turtle-torque tur H T) s p ps poly extras))

(define (yaw t φ)
  (match-define (turtle-state tur s p ps poly es) t)
  (turtle-state (turtle-yaw tur φ) s p ps poly es))
(define (pitch t φ)
  (match-define (turtle-state tur s p ps poly es) t)
  (turtle-state (turtle-pitch tur φ) s p ps poly es))
(define (roll t φ)
  (match-define (turtle-state tur s p ps poly es) t)
  (turtle-state (turtle-roll tur φ) s p ps poly es))

(define (add-to-poly-stack pt poly)
  (if (empty? poly) poly (cons (cons pt (first poly)) (rest poly))))

(define (out:move t d)
  (match-define (turtle-state tur s p ps poly es) t)
  (define t* (turtle-move tur d))
  (define pt (turtle->point t*))
  (turtle-state t* s (cons pt p) ps (add-to-poly-stack pt poly) es))
(define (move/no-poly t d)
  (match-define (turtle-state tur s p ps poly es) t)
  (define t* (turtle-move tur d))
  (define pt (turtle->point t*))
  (turtle-state t* s (cons pt p) ps poly es))

(define (shift t d)
  (match-define (turtle-state tur s p ps poly es) t)
  (define t* (turtle-move tur d))
  (define pt (turtle->point t*))
  (turtle-state t* s p ps (add-to-poly-stack pt poly) es))
(define (save-vertex t)
  (match-define (turtle-state tur s p ps poly es) t)
  (define pt (turtle->point tur))
  (turtle-state tur s p ps (add-to-poly-stack pt poly) es))

(define (save t)
  (match-define (turtle-state tur s p ps poly es) t)
  (turtle-state tur (cons tur s) (list (turtle->point tur)) (cons p ps) poly es))
(define (restore t)
  (match-define (turtle-state _ (cons tur s) p ps poly es) t)
  (turtle-state tur s (list (turtle->point tur)) (cons p ps) poly es))

(define (start-poly t)
  (match-define (turtle-state tur s p ps poly es) t)
  (turtle-state tur s p ps (cons empty poly) es))
(define (end-poly t)
  (match-define (turtle-state tur s p ps poly es) t)
  (unless (pair? poly) (error "attempted to create a polygon without starting one, at " tur))
  (turtle-state tur s p ps (rest poly) (cons (make-polygon (first poly)) es)))

(define (draw t [color-vec #f])
  (match-define (turtle-state _ _ points points-stack poly extras) t)
  (freeze
   (add-extra-picts
    extras
    (for/fold ([p empty-pict3d])
              ([points (in-list (cons points points-stack))])
      (combine p (draw-points points color-vec)))
    color-vec)))

(define (shift-color-index t d)
  (match-define (turtle-state tur s p ps polys es) t)
  (turtle-state (turtle-change-color tur d) s p ps polys es))

(define (grow t d)
  (match-define (turtle-state tur s p ps polys es) t)
  (turtle-state (turtle-grow tur d) s p ps polys es))

(define (set-width t w)
  (match-define (turtle-state (turtle pos d u _ c) s p ps polys extras) t)
  (turtle-state (turtle pos d u w c) s p ps polys extras))

(define (insert-pict t pict)
  (match-define (turtle-state tur s p ps polys extras) t)
  (turtle-state tur s p ps polys (cons (turtle->extras tur pict) extras)))

(define (reorient-to-up t)
  (match-define (turtle-state (turtle pos d u w c) s p ps polys extras) t)
  (define l (dir-normalize (dir-cross -z d)))
  (define u* (dir-cross d l))
  (turtle-state (turtle pos d u* w c) s p ps polys extras))

(define (turtle->point t)
  (match-define (turtle p _ _ width color) t)
  (make-point p width color))

(define (turtle->extras t p)
  (match-define (turtle pt d _ _ _) t)
  (make-extras pt d p))

(define current-texturing
  (make-parameter (list (vector default-color) default-emitted)))
(define (set-rendering-config! width height #:ambiance? [ambiance? #f]
                               #:debug? [debug? #f]
                               #:background [background (rgba "white" 0)]
                               #:emit [emit default-emitted])
  (current-pict3d-background background)
  (current-pict3d-add-indicators? debug?)
  (current-pict3d-add-grid?)
  (cond
    [ambiance?
     (current-texturing (list (vector default-color) emit))
     (current-pict3d-ambient (emitted "white" 1))
     (current-pict3d-add-sunlight? #t)]
    [else
     (current-texturing (list (vector (rgba "black")) (emitted "black" 0)))
     (current-pict3d-ambient (emitted "black" 0))
     (current-pict3d-add-sunlight? #f)])
  (current-pict3d-width width)
  (current-pict3d-height height))

(define (unit-dir? d)
  (dir=? d (dir-normalize d)))

(define (ortho? v k)
    (0.00000001 . > . (abs (dir-dot v k))))
;; poor mans non-gimble locking position+rotation
(struct turtle (pos dir up width color-index)
  #:transparent)

(define/contract (make-turtle pos dir up w c)
  (->i ([pos dir?]
        [dir unit-dir?]
        [up unit-dir?]
        [width (>=/c 0)]
        [color-index exact-nonnegative-integer?])
       #:pre (up dir) (ortho? up dir)
       [result turtle?])
  (turtle pos dir up w c))
;; pos : Pos, di Dir, up:Dir
;; up and dir *must* be orthoginal
;; up and dir must be unit vectors


(define pipe-cache (make-hash))
(define (make-pipe length width color)
  (define c (or color (first (current-texturing))))
  (hash-ref!
   pipe-cache
   (list*
    length
    width
    c
    (second (current-texturing)))
   (lambda ()
     (define line-length length)
     (define line-size (max width 1/64))
     (define ls/2 (/ line-size 2))
     (with-color c
       (with-emitted (second (current-texturing))
         (define cap (sphere origin ls/2))
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

(define (add-extra-picts extrs pict colors)
  (for/fold ([p pict])
            ([e (in-list extrs)])
    (combine
     p
     (match e
       [(extras pt d ict)
        (shift-pict pt d ict)]
       [(polygon points)
        (draw-poly points colors)]))))

(define (draw-points pts colors)
  (if (empty? pts)
      empty-pict3d
      (for/fold ([p empty-pict3d])
                ([start (in-list pts)]
                 [end (in-list (rest pts))])
        (match-define (point d w c) start)
        (combine p (dirs->pipe d (point-dir end) w (lookup-color colors c))))))

(define (draw-poly points colors)
  (define (make-vertex dir c)
    (vertex (pos+ origin dir) #:color (lookup-color colors c) #:emitted (second (current-texturing))))
  (match points
    [(or (list) (list _) (list _ _)) empty-pict3d]
    [(list (point dir width color-index) pts ...)
     (define v1 (make-vertex dir color-index))
     (for/fold ([p empty-pict3d])
               ([left (in-list pts)]
                [right (in-list (rest pts))])
       (match-define (point dir2 _ color-index2) left)
       (match-define (point dir3 _ color-index3) right)
       (combine
        p
        (triangle
         #:back? #t
         v1
         (make-vertex dir2 color-index2)
         (make-vertex dir3 color-index3))
        (triangle
         v1
         (make-vertex dir2 color-index2)
         (make-vertex dir3 color-index3))))]))

(define (lookup-color v c)
  (and v (vector-ref v (modulo c (vector-length v)))))

;;;; helpers
(define (dirs->pipe start end w c)
  (define dir (dir- end start))
  (define pipe (make-pipe (dir-dist dir) w c))
  (shift-pict start dir pipe))

(define (shift-pict point dir pict)
  (define-values (yaw pitch) (dir->angles dir))
  (define crossdir (dir-cross +x dir))
  (define axis (dir-cross +x dir))
  (define angle (angle-between +x dir))
  (transform
   pict
   (affine-compose
    (move point)
    (if (zero-dir? axis)
        (if (= 180.0 angle) (3d:rotate +z angle) identity-affine)
        (3d:rotate axis angle)))))

(define (dir=? d1 d2)
  (and (=-ish? (dir-dx d1) (dir-dx d2))
       (=-ish? (dir-dy d1) (dir-dy d2))
       (=-ish? (dir-dz d1) (dir-dz d2))))

(define (angle-between v k)
  (radians->degrees
   (acos
    (max
     -1
     (min
      1
      (/ (dir-dot v k)
         (* (dir-dist v) (dir-dist k))))))))

(define (zero-dir? axis)
  (dir=? axis zero-dir))

(define (zero-ish? x)
  (=-ish? 0 x))

(define (=-ish? x y)
  (define threshold 0.0000001)
  (> threshold (- x y) (- threshold)))

;; rotate the turtle left/right by φ degrees
(define (turtle-yaw t φ)
  (match-define (turtle pos dir up width color-index) t)
  (define dir* (dir-normalize (rotate dir up φ)))
  (make-turtle pos dir* up width color-index))

;; pitch the turtle up or down φ degrees
(define (turtle-pitch t φ)
  (match-define (turtle pos dir up width color-index) t)
  (turtle-rotate t (dir-cross dir up) φ))

;; pitch the turtle by φ degrees
(define (turtle-roll t φ)
  (match-define (turtle pos dir up width color-index) t)
  (define up* (dir-normalize (rotate up dir φ)))
  (make-turtle pos dir up* width color-index))


(define (turtle-rotate t axis φ)
  (match-define (turtle pos dir up width color-index) t)
  (define dir* (dir-normalize (rotate dir axis φ)))
  (define up* (dir-normalize (rotate up axis φ)))
  (make-turtle pos dir* up* width color-index))

(define (turtle-move t d)
  (match-define (turtle pos dir up width color-index) t)
  (define pos* (dir+ pos (dir-scale dir d)))
  (make-turtle pos* dir up width color-index))

(define (turtle-grow t d)
  (match-define (turtle pos dir up width color-index) t)
  (make-turtle  pos dir up (+ width (* width d)) color-index))

(define (turtle-change-color t d)
  (match-define (turtle pos dir up width color-index) t)
  (make-turtle  pos dir up width (+ color-index d)))

(define (turtle-torque tur dist T)
  (match-define (turtle pos dir** up width color-index) tur)
  (define dir (dir-scale dir** dist))
  (define axis (dir-normalize (dir-cross dir T)))
  (cond
    [(implies axis (zero-dir? axis))
     tur]
    [else
     (define dir* (dir-normalize (dir+ dir T)))
     (define rotated (angle-between dir* dir))
     (define up* (rotate up axis rotated))
     (turtle pos dir* up* width color-index)]))



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

(define starting-turtle (out:make-turtle zero-dir +x +z))
