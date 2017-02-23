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
 grow
 shift-color-index
 draw
 starting-turtle
 insert-pict
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
  (turtle-state turtle* empty (list (turtle->point turtle*)) empty empty))

(struct turtle-state (turtle stack points points-stack extra-picts)
  #:transparent)

(struct point (dir width color-index)
  #:transparent
  #:extra-constructor-name make-point)

(struct extras (point dir pict)
  #:transparent
  #:extra-constructor-name make-extras)

(define (yaw t φ)
  (match-define (turtle-state tur s p ps es) t)
  (turtle-state (turtle-yaw tur φ) s p ps es))
(define (pitch t φ)
  (match-define (turtle-state tur s p ps es) t)
  (turtle-state (turtle-pitch tur φ) s p ps es))
(define (roll t φ)
  (match-define (turtle-state tur s p ps es) t)
  (turtle-state (turtle-roll tur φ) s p ps es))

(define (out:move t d)
  (match-define (turtle-state tur s p ps es) t)
  (define t* (turtle-move tur d))
  (turtle-state t* s (cons (turtle->point t*) p) ps es))

(define (save t)
  (match-define (turtle-state tur s p ps es) t)
  (turtle-state tur (cons tur s) (list (turtle->point tur)) (cons p ps) es))
(define (restore t)
  (match-define (turtle-state _ (cons tur s) p ps es) t)
  (turtle-state tur s (list (turtle->point tur)) (cons p ps) es))

(define (draw t [color-vec #f])
  (match-define (turtle-state _ _ points points-stack extras) t)
  (freeze
   (add-extra-picts
    extras
    (for/fold ([p empty-pict3d])
              ([points (in-list (cons points points-stack))])
      (combine p (draw-points points color-vec))))))

(define (shift-color-index t d)
  (match-define (turtle-state tur s p ps es) t)
  (turtle-state (turtle-change-color tur d) s p ps es))

(define (grow t d)
  (match-define (turtle-state tur s p ps es) t)
  (turtle-state (turtle-grow tur d) s p ps es))

(define (insert-pict t pict)
  (match-define (turtle-state tur s p ps extras) t)
  (turtle-state tur s p ps (cons (turtle->extras tur pict) extras)))

(define (turtle->point t)
  (match-define (turtle p _ _ width color) t)
  (make-point p width color))

(define (turtle->extras t p)
  (match-define (turtle pt d _ _ _) t)
  (make-extras pt d p))

(define current-texturing
  (make-parameter (list (vector default-color) default-emitted)))
(define (set-rendering-config! width height #:ambiance? [ambiance? #f] #:debug? [debug? #f])
  (current-pict3d-background (rgba "white" 0))
  (current-pict3d-add-indicators? debug?)
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
(define (make-pipe width color)
  (define c (or color (first (current-texturing))))
  (hash-ref!
   pipe-cache
   (list* width
          c
          (second (current-texturing)))
   (lambda ()
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

(define (add-extra-picts extrs pict)
  (for/fold ([p pict])
            ([e (in-list extrs)])
    (match-define (extras pt d ict) e)
    (combine p (shift-pict pt d ict))))

(define (draw-points pts colors)
  (if (empty? pts)
      empty-pict3d
      (for/fold ([p empty-pict3d])
                ([start (in-list pts)]
                 [end (in-list (rest pts))])
        (match-define (point d w c) start)
        (define pipe (make-pipe w (lookup-color colors c)))
        (combine p (dirs->pipe d (point-dir end) pipe)))))

(define (lookup-color v c)
  (and v (vector-ref v (modulo c (vector-length v)))))

;;;; helpers
(define (dirs->pipe start end pipe)
  (define dir (dir- end start))
  (shift-pict start dir pipe))

(define (shift-pict point dir pict)
  (define-values (yaw pitch) (dir->angles dir))
  (define crossdir (dir-cross +x dir))
  (define axis (dir-cross +x dir))
  (define angle
    (radians->degrees
     (acos
      (/ (dir-dot +x dir)
         (* (dir-dist +x) (dir-dist dir))))))
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
  (define axis (dir-cross dir up))
  (define dir* (dir-normalize (rotate dir axis φ)))
  (define up* (dir-normalize (rotate up axis φ)))
  (make-turtle pos dir* up* width color-index))

;; pitch the turtle by φ degrees
(define (turtle-roll t φ)
  (match-define (turtle pos dir up width color-index) t)
  (define up* (dir-normalize (rotate up dir φ)))
  (make-turtle pos dir up* width color-index))

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
