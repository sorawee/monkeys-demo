#lang racket

(require 2htdp/universe
         2htdp/image)

(define banana-img (bitmap/file "banana.png"))
(define monkey-img (bitmap/file "monkey.png"))

(struct state (vx ; horiz speed
               vy ; vert speed
               x ; horiz pos
               y ; vert pos
               ))

(define width 800)
(define height 500)
(define left-monkey-x-pos 100)
(define right-monkey-x-pos 700)
(define ground-height 400)
(define gravity 1)
(define update-delay 1/33)

;; init-state :: state
(define init-state (state 5 25 left-monkey-x-pos ground-height))

;; handle-draw :: state -> image
(define (handle-draw s)
  (match-define (state _ _ x y) s)
  (let* ([scene (empty-scene width height)]
         [scene (place-image banana-img
                             x y
                             scene)]
         [scene (place-image monkey-img
                             left-monkey-x-pos ground-height
                             scene)]
         [scene (place-image monkey-img
                             right-monkey-x-pos ground-height
                             scene)])
    scene))

;; handle-tick :: state -> state
(define (handle-tick s)
  (match-define (state vx vy x y) s)
  (state vx
         (- vy gravity)
         (+ x vx)
         (- y vy)))

;; handle-mouse :: state number number mouse-event -> state
(define (handle-mouse s mx my evt)
  (cond
    [(equal? evt "button-down") init-state]
    [else s]))

(big-bang init-state
  [to-draw handle-draw]
  [on-tick handle-tick update-delay]
  [on-mouse handle-mouse]
  [name "Monkey Banana Game"])
