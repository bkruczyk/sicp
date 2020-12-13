#lang sicp

(#%require racket/trace)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; excercise 1.34

;; (f f)
;; (f 2)
;; (2 2)

;; (define (f g) (g 2))

;; (^ p 2) = (+ 1 p)
;; p = (/ (+ 1 p) p)
;; p = (* (+ 1 p) (/ 1 p))
;; p = 1/p + 1
;;

(define (average x y) (/ (+ x y) 2))
;; (define (sqrt x)
;;   (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; (define phi
;;   (fixed-point
;;    (lambda (x) (+ 1 (/ 1 x)))
;;    1.0))

;; (define exp1000
;;   (fixed-point
;;    (lambda (x) (/
;;                 (log 1000)
;;                 (log x))) 2.0))

;; (define exp1000-damp
;;   (fixed-point
;;    (lambda (x) (average x (/
;;                            (log 1000)
;;                            (log x)))) 2.0))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= 0 i)
        result
        (iter (dec i) (/ (n i) (+ result (d i))))))
  (iter (dec k) (/ (n k) (d k))))

(define (cont-frac n d k)
  (define (recur i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recur (inc i))))))
  (recur 1))

(define (next i)
  (cond ((= i 1) 1)
        ((= i 2) 2)
        (else
         (if (= 0 (remainder (- i 2) 3))
             (+ 2 (* (/ (- i 2) 3) 2))
             1))))

(define (display-serie f n)
  (define (rec i)
    (display (f i)) (display ", ")
    (if (= i n)
        (newline)
        (rec (inc i))))
  (rec 1))


(define (euler k)
  (cont-frac (lambda (i) 1.0) next k))

(define (tan-cf x k)
  (cont-frac (lambda (i)
                (if (= i 1) x (* x x -1)))
              (lambda (i) (- (* 2.0 i) 1))
              k))

;; 1.3.4

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (square x) (* x x))

;; ((average-damp square) 10)

;; (define (sqrt x)
;;   (fixed-point
;;    (average-damp
;;     (lambda (y) (/ x y)))
;;    1.0))

(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y)
      (/ x (square y))))
   1.0))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

;; (define (sqrt x)
;;   (newtons-method
;;    (lambda (y)
;;      (- (square y) x))
;;    1.0))

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))

;; (define (sqrt x)
;;   (fixed-point-of-transform
;;    (lambda (y) (/ x y))
;;    average-damp
;;    1.0))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

;; 1.40
(define (cubic a b c)
  (lambda (x)
    (+
     (* x x x)
     (* a x x)
     (* b x)
     c)))

;; (newtons-method (cubic 1 0 0) 1.0)

;; 1.41
(define (inc x) (+ x 1))
(define (double x)
  (lambda (y) (x (x y))))

;; (double double)
;; y. (double (double y))
;; inc. (double (double inc))
;; 1.     ((double (double inc)) 1)
;; (double (inc (inc x)))
;; (inc (inc (inc (inc x))))
;; (double (inc (inc (inc (inc x)))))
;; (inc (inc (inc (inc (inc (inc (inc (inc x))))))))

;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; 1.43

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))


;; 1.44
(define (smooth f)
  (lambda (x)
    (/ (+
        (f (- x dx))
        (f x)
        (f (+ x dx)))
       3)))

;; (((repeated smooth 5) square) 2)

;; 1.45
;; (define x 16)
;; (fixed-point
;;  ((repeated average-damp 2)
;;   (lambda (y) (/ x (* y y y))))
;;  1.0)
;; (define x 65536)
;; (fixed-point
;;  ((repeated average-damp 4)
;;   (lambda (y) (/ x (* y y y y y y y y y y y y y y y))))
;;  1.0)

(define (nth-root x n)
  (define (pow x n)
    (if (= n 1)
        x
        (* x (pow x (dec n)))))

  (fixed-point
   ((repeated average-damp (floor (log n 2)))
    (lambda (y)
      (/ x (pow y (dec n)))))
   1.0))

;; 1.46


(define (iterative-improve is-good-enough improve-guess)
  (define (recur prev-guess guess)
    (if (is-good-enough prev-guess guess)
        guess
        (recur guess (improve-guess guess))))
  (lambda (guess)
    (recur guess (improve-guess guess))))

(define (good-enough? a b) (< (abs (- a b)) 0.0001))

(define (sqrt-improve x)
  ((iterative-improve
    good-enough?
    (average-damp (lambda (y) (/ x y))))
   1.0))

(define (fixed-point-improve f)
  ((iterative-improve
    good-enough?
    (lambda (x) (f x)))
   1.0))
;; (fixed-point-improve (average-damp (lambda (y) (/ 4 y))))
