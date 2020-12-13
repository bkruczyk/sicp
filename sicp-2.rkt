#lang sicp

(#%require racket/trace)


;; (define x (cons 1 2))
;; (define y (cons 3 4))

;; (define z (cons x y))

;; ;; (define (make-rat n d) (cons n d))
;; (define (make-rat n d)
;;   (let ((g (gcd n d)))
;;     (cons (/ n g)
;;           (/ d g))))
;; (define (numer x) (car x))
;; (define (denom x) (cdr x))
;; (define (add-rat x y)
;;   (make-rat (+ (* (numer x) (denom y))
;;                (* (numer y) (denom x)))
;;             (* (denom x) (denom y))))

;; (define (sub-rat x y)
;;   (make-rat (- (* (numer x) (denom y))
;;                (* (numer y) (denom x)))
;;             (* (denom x) (denom y))))

;; (define (mul-rat x y)
;;   (make-rat (* (numer x) (numer y))
;;             (* (denom x) (denom y))))

;; (define (div-rat x y)
;;   (make-rat (* (numer x) (denom y))
;;             (* (denom x) (numer y))))

;; (define (equal-rat? x y)
;;   (= (* (numer x) (denom y))
;;      (* (numer y) (denom x))))

;; (define (print-rat x)
;;   (newline)
;;   (display (numer x))
;;   (display "/")
;;   (display (denom x)))

;; (define one-half (make-rat 1 2))
;; (define one-third (make-rat 1 3))

;; ;; excercise 2.1

;; (define (make-rat-norm n d)
;;   (if (< (/ n d) 0)
;;       (make-rat (- (abs n)) (abs d))
;;       (make-rat (abs n) (abs d))))

;; ;; excercise 2.2

;; (define (make-point x y)
;;   (cons x y))
;; (define (x-point point)
;;   (car point))
;; (define (y-point point)
;;   (cdr point))

;; (define (print-point p)
;;   (newline)
;;   (display "(")
;;   (display (x-point p))
;;   (display ",")
;;   (display (y-point p))
;;   (display ")"))

;; (define (make-segment a b)
;;   (cons a b))
;; (define (start-segment segment)
;;   (car segment))
;; (define (end-segment segment)
;;   (cdr segment))

;; (define (midpoint-segment segment)
;;   (make-point (/ (+
;;                   (x-point (start-segment segment))
;;                   (x-point (end-segment segment)))
;;                  2)
;;               (/ (+
;;                   (y-point (start-segment segment))
;;                   (y-point (end-segment segment)))
;;                  2)))

;; ;; excercise 2.3

;; (define (make-rectangle a b)
;;   (cons a b))

;; (define (segment-length segment)
;;   (let ((a (-
;;             (x-point (end-segment segment))
;;             (x-point (start-segment segment))))
;;         (b (-
;;             (y-point (end-segment segment))
;;             (y-point (start-segment segment)))))
;;     (sqrt (+ (* a a) (* b b)))))

;; (define (a-side rectangle)
;;   (car rectangle))
;; (define (b-side rectangle)
;;   (cdr rectangle))
;; (define (perimeter rectangle)
;;   (+ (* 2 (segment-length (a-side rectangle)))
;;      (* 2 (segment-length (b-side rectangle)))))
;; (define (area rectangle)
;;   (* (segment-length (a-side rectangle))
;;      (segment-length (b-side rectangle))))

;; (define rectangle
;;   (make-rectangle
;;    (make-segment (make-point 0 0) (make-point 0 2))
;;    (make-segment (make-point 0 0) (make-point 2 0))))

;; ;; second representation could use points instead of segments in constructor
;; ;; (define (make-rectangle a b c d) ...)
;; ;; a-side and b-side selectors should be modified accordingly

;; ;; excercise 2.4
;; (define (cons x y)
;;   (lambda (m) (m x y)))

;; (define (car z)
;;   (z (lambda (p q) p)))

;; (define (cdr z)
;;   (z (lambda (p q) q)))

;; excercise 2.5

(define (pow a n)
  (if (= n 0) 1
      (* a (pow a (dec n)))))

;; (define (cons x y)
;;   (* (pow 2 x) (pow 3 y)))

;; example (cons 2 3)
;; 2^2 * 3^3 = 4 * 27 = 108
;; 2 * 2 * 3 * 3 * 3  = 108
;; 2 * 3 * 3 * 3 = 108 / 2 = 54
;; 3 * 3 * 3     = (108 / 2) / 2 = 27
;;
;; then 27 mod 2 is not 0, so car part is 2
;; similar thing can be done for cdr by dividing by 3

;; (define (factor z n a)
;;   (if (= 0 (remainder z n))
;;       (factor (/ z n) n (inc a))
;;       a))

;; (define (car z) (factor z 2 0))
;; (define (cdr z) (factor z 3 0))

;; (define zero (lambda (f) (lambda (x) x)))

;; (define (add-1 n)
;;   (lambda (f) (lambda (x) (f ((n f) x)))))

;; (add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f (((lambda (z) (lambda (g) g)) f) x)))))
;; (lambda (f) (lambda (x) (f ((lambda (g) g) x))))
;; (lambda (f) (lambda (x) (f x)))

;; (add-1 (add-1 zero))
;; (lambda (a) (lambda (b) (a (((add-1 zero) a) b))))
;; (lambda (a) (lambda (b) (a (((lambda (f) (lambda (x) (f x))) a) b))))
;; (lambda (a) (lambda (b) (a (a b)))

;; (add-1 (add-1 (add-1 zero)))
;; ... probably
;; (lambda (d) (lambda (e) (d (d (d e)))))

;; (define one (lambda (f) (lambda (x) (f x))))
;; (define two (lambda (f) (lambda (x) (f (f x)))))

;; I accidentaly implemented multiplication XDDD
;; (define (plus a b)
;;   (lambda (f)
;;     (lambda (x) ((a (b f)) x))))

;; (define (real-plus a b)
;;   (lambda (f)
;;     (lambda (x)
;;       ((a f) ((b f) x)))))

;; ;; excercise 2.7

;; (define (make-interval a b) (cons a b))

;; (define (upper-bound interval)
;;   (cdr interval))

;; (define (lower-bound interval)
;;   (car interval))

;; ;; excercise 2.8
;; ;; ???

;; (define (add-interval x y)
;;   (make-interval (+ (lower-bound x)
;;                     (lower-bound y))
;;                  (+ (upper-bound x)
;;                     (upper-bound y))))

;; (define (mul-interval x y)
;;   (let ((p1 (* (lower-bound x)
;;                (lower-bound y)))
;;         (p2 (* (lower-bound x)
;;                (upper-bound y)))
;;         (p3 (* (upper-bound x)
;;                (lower-bound y)))
;;         (p4 (* (upper-bound x)
;;                (upper-bound y))))
;;     (make-interval (min p1 p2 p3 p4)
;;                    (max p1 p2 p3 p4))))

;; (define (div-interval x y)
;;   (mul-interval x
;;                 (make-interval
;;                  (/ 1.0 (upper-bound y))
;;                  (/ 1.0 (lower-bound y)))))

;; ;; excercise 2.17

;; (define (last-pair xs)
;;   (if (null? (cdr xs))
;;       (car xs)
;;       (last-pair (cdr xs))))

;; ;; excercise 2.18

;; (define (reverse xs)
;;   (if (null? xs)
;;       xs
;;       (append
;;        (reverse (cdr xs))
;;        (list (car xs)))))

;; ;; excercise 2.19
;; (define us-coins
;;   (list 50 25 10 5 1))

;; (define uk-coins
;;   (list 100 50 20 10 5 2 1 0.5))

;; (define (cc amount coin-values)
;;   (cond ((= amount 0) 1)
;;         ((or (< amount 0)
;;              (no-more? coin-values))
;;          0)
;;         (else
;;          (+ (cc amount (except-first-denomination coin-values))
;;             (cc (- amount (first-denomination
;;                            coin-values))
;;                    coin-values)))))

;; (define (first-denomination coin-values)
;;   (car coin-values))

;; (define (except-first-denomination coin-values)
;;   (cdr coin-values))

;; (define (no-more? coin-values)
;;   (null? coin-values))

;; excercise 2.20

(define (recur parity xs)
  (cond ((null? xs) xs)
        ((parity (car xs)) (cons (car xs) (recur parity (cdr xs))))
        (else (recur parity (cdr xs)))))
(define (same-parity x . xs)
  (cons x (recur (if (even? x) even? odd?) xs)))


