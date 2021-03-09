#lang racket

(require 2htdp/universe
         2htdp/image)

(struct banana-state (vx ; number -- horiz speed
                      vy ; number -- vert speed
                      x ; number -- horiz pos
                      y ; number -- vert pos
                      ))

(struct state (banana ; banana-state or #f
               speed ; number -- controllable speed
               angle ; number -- controllable angle
               ))

(define width 800)
(define height 500)
(define left-monkey-x-pos 100)
(define right-monkey-x-pos 700)
(define ground-height 400)
(define gravity 1)
(define update-delay 1/33)

(define speed 4)
(define angle 45)

(define high-speed 10)
(define low-speed 1)

(define high-angle 89)
(define low-angle 1)

(define text-size 18)
(define text-x-pos 40)
(define text-y-pos 20)

(define banana-img (bitmap/file "banana.png"))
(define monkey-img (bitmap/file "monkey.png"))
(define arrow-img (rotate -90
                          (above (triangle 12 "solid" "tan")
                                 (rectangle 4 20 "solid" "tan")
                                 (rectangle 4 150 "solid" "transparent"))))
(define base-scene
  (let* ([scene (empty-scene width height)]
         [scene (place-image/align (rectangle width (- height ground-height)
                                              "solid" "brown")
                                   0 ground-height
                                   "left" "top"
                                   scene)])
    scene))


;; throwing-state :: number number -> state
(define (throwing-state speed angle)
  (state (banana-state (* 5 speed (cos (* (/ pi 180) angle)))
                       (* 5 speed (sin (* (/ pi 180) angle)))
                       left-monkey-x-pos
                       ground-height)
         speed angle))

;; handle-draw :: state -> image
(define (handle-draw s)
  (match-define (state banana speed angle) s)
  (let* ([scene base-scene]
         [scene (match banana
                  [(banana-state _ _ x y) (place-image banana-img x y scene)]
                  [_ scene])]
         [scene (place-image monkey-img
                             left-monkey-x-pos ground-height
                             scene)]
         [scene (place-image monkey-img
                             right-monkey-x-pos ground-height
                             scene)]
         [scene (place-image/align (text (format (string-append "Speed: ~a\n"
                                                                "Angle: ~a\n")
                                                 (~r speed #:precision 1)
                                                 angle)
                                         text-size "blue")
                                   text-x-pos text-y-pos
                                   "left" "top"
                                   scene)]
         [scene (place-image (rotate angle arrow-img)
                             left-monkey-x-pos ground-height
                             scene)])
    scene))

;; banana-moving? :: state -> boolean?
(define (banana-moving? s)
  (match s
    [(state (banana-state _ _ _ y) _ _) (<= y ground-height)]
    [(state #f _ _) #f]))

;; handle-tick :: state -> state
(define (handle-tick s)
  (struct-copy state s
               [banana
                (cond
                  [(banana-moving? s)
                   (match-define (state (banana-state vx vy x y) _ _) s)
                   (banana-state vx (- vy gravity)
                                 (+ x vx) (- y vy))]
                  [else #f])]))

;; clip :: number (number -> number) number number -> number
(define (clip x f l r)
  (define result (f x))
  (cond
    [(<= l result r) result]
    [else x]))

;; handle-key :: state key-event -> state
(define (handle-key s evt)
  (cond
    [(key=? evt " ")
     (cond
       [(banana-moving? s) s]
       [else (throwing-state (state-speed s) (state-angle s))])]
    [(or (key=? evt "-") (key=? evt "="))
     (struct-copy state s
                  [speed (clip (state-speed s)
                               (λ (x) ((if (key=? evt "-") - +) x 1/5))
                               low-speed high-speed)])]
    [(or (key=? evt "down") (key=? evt "up"))
     (struct-copy state s
                  [angle (clip (state-angle s)
                               (λ (x) ((if (key=? evt "down") - +) x 5))
                               low-angle high-angle)])]
    [else s]))

(big-bang (state #f speed angle)
  [to-draw handle-draw]
  [on-tick handle-tick update-delay]
  [on-key handle-key]
  [name "Monkey Banana Game"])
