#lang racket

(#%require sicp-pict)

(define wave einstein)
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

;; (define (flipped-pairs painter)
;;   (let ((painter2 (beside painter (flip-vert painter))))
;;     (below painter2 painter2)))

;; (define wave4 (flipped-pairs wave))

;; (define (right-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (right-split painter (- n 1))))
;;         (beside painter (below smaller smaller)))))

;; ;; excercise 2.44
;; (define (up-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (up-split painter (- n 1))))
;;         (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right
                                   right))
              (corner (corner-split painter
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right
                         corner))))))

;; (define (square-limit painter n)
;;   (let ((quarter (corner-split painter n)))
;;     (let ((half (beside (flip-horiz quarter)
;;                         quarter)))
;;       (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter)
                       (tr painter)))
          (bottom (beside (bl painter)
                          (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4
         (square-of-four identity
                         flip-vert
                         identity
                         flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4
         (square-of-four flip-horiz
                         identity
                         rotate180
                         flip-vert)))
    (combine4 (corner-split painter n))))

;; excercise 2.45
(define (split x y)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split x y) painter (- n 1))))
          (x painter (y smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))
;; excercise 2.46

;; (define (make-vect x y) (list x y))
;; (define (xcor-vect v) (car v))
;; (define (ycor-vect v) (cadr v))

;; (define (add-vect v w)
;;   (make-vect (+ (xcor-vect v) (xcor-vect w))
;;              (+ (ycor-vect v) (ycor-vect w))))

;; (define (sub-vect v w)
;;   (make-vect (- (xcor-vect v) (xcor-vect w))
;;              (- (ycor-vect v) (ycor-vect w))))

;; (define (scale-vect s v)
;;   (make-vect (* s (xcor-vect v)) (* s (ycor-vect))))

;; excercise 2.47
;; (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))

;; (define (origin-frame frame) (car frame))
;; (define (edge1-frame frame) (cadr frame))
;; (define (edge2-frame frame) (cadr (cdr frame)))

;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))

;; (define (origin-frame frame) (car frame))
;; (define (edge1-frame frame) (cadr frame))
;; (define (edge2-frame frame) (cdr (cdr frame)))

;; (define (frame-coord-map frame)
;;   (lambda (v)
;;     (add-vect
;;      (origin-frame frame)
;;      (add-vect
;;       (scale-vect (xcor-vect v)
;;                   (edge1-frame frame))
;;       (scale-vect (ycor-vect v)
;;                   (edge2-frame frame))))))

;; (define (for-each f items)
;;   (if (null? items)
;;       #f
;;       (or (and (f (car items)) #f)
;;           (for-each f (cdr items)))))


;; excercise 2.48

;; (define (make-segment v w) (list v w))
;; (define (start-segment s) (car s))
;; (define (end-segment s) (cadr s))

;; excercise 2.49
(define outline-painter
  (segments->painter (vects->segments (list
                                       (make-vect 0 0)
                                       (make-vect 0 1)
                                       (make-vect 1 1)
                                       (make-vect 1 0)
                                       (make-vect 0 0)))))
(define x-painter
  (segments->painter (list
                      (make-segment (make-vect 0 0) (make-vect 1 1))
                      (make-segment (make-vect 0 1) (make-vect 1 0)))))
(define diamond-painter
  (segments->painter (vects->segments
                      (list
                       (make-vect 0   0.5)
                       (make-vect 0.5 1)
                       (make-vect 1   0.5)
                       (make-vect 0.5 0)
                       (make-vect 0.5 0)
                       (make-vect 0   0.5)))))

;; (define wave-painter nope xD)

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

;; excercise 2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rot90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rot180 painter)
  (rot90 (rot90 painter)))
(define (rot270 painter)
  (rot90 (rot90 (rot90 painter))))

;; excercise 2.51

(define (below% p1 p2)
  (lambda (frame)
    ((transform-painter p1
                        (make-vect 0.0 0.5)
                        (make-vect 0.0 1.0)
                        (make-vect 1.0 0.0)) frame)
    ((transform-painter p2
                        (make-vect 0.0 0.0)
                        (make-vect 0.0 0.5)
                        (make-vect 1.0 0.0)) frame)))

(define (below# p1 p2)
  (rot90 (rot180 (beside (rot90 p1) (rot90 p2)))))

;; excercise 2.52
;; I skipped this one since some of those primitve constructs are provided by sicp-pict lang
