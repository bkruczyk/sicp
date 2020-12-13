#lang sicp

(#%require racket/trace)

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; excercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; excercise 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; (new-if (= 2 3) 0 5)

;; (new-if (= 1 1) 0 5)

;; (sqrt-iter 1 2)

;; excercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 1 10)
;; (A 2 4)
;; (A 3 3)

;; (define (f n) (A 0 n))
;; (define (g n) (A 1 n))
;; (define (h n) (A 2 n))
;; (define (k n) (* 5 n n))

;; excercise 1.11

;; (define (f1 n)
;;   (cond ((< n 3) n)
;;         (else
;;          (+
;;           (f (- n 1))
;;           (* 2 (f (- n 2)))
;;           (* 3 (f (- n 3)))))))

;; (define (f2 n)
;;   (cond ((< n 3) n)
;;         (else
;;          (f-iter n 2 2 1 0))))

;; (define (f-iter n x a1 a2 a3)
;;   (cond ((= n x) a1)
;;         (else
;;          (f-iter n (+ x 1) (+ a1 (* 2 a2) (* 3 a3)) a1 a2))))


;; excercise 1.12

(define (pascal n m)
  (cond ((= n 1) 1)
        ((= n 2) 1)
        ((= m 1) 1)
        ((= m n) 1)
        (else
         (+ (pascal (dec n) (dec m)) (pascal (dec n) m)))))

(define (pascal-row n)
  (define (pascal-row n m xs)
    (cond ((= m 0) xs)
          (else (cons (pascal n m) (pascal-row n (dec m) xs)))))
  (pascal-row n n '()))

(define (pascal-rows n)
  (cond ((= n 1) (list (pascal-row 1)))
        (else (cons (pascal-row n) (pascal-rows (dec n))))))


;; excercise 1.16
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

;; excercise 1.18

(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (fast-mult n m)
  (fast-mult-iter n m 0))
(define (fast-mult-iter n m acc)
  (cond ((= m 0) acc)
        ((even? m) (fast-mult-iter (double n) (halve m) acc))
        (else (fast-mult-iter n (- m 1) (+ acc n)))))


;; exercise 1.19
;; (define (fib n)
;;   (fib-iter 1 0 0 1 n))

;; (define (fib-iter a b p q count)
;;   (cond ((= count 0)
;;          b)
;;         ((even? count)
;;          (fib-iter a
;;                    b
;;                    ⟨??⟩  ;compute p'
;;                    ⟨??⟩  ;compute q'
;;                    (/ count 2)))
;;         (else
;;          (fib-iter (+ (* b q)
;;                       (* a q)
;;                       (* a p))
;;                    (+ (* b p)
;;                       (* a q))
;;                    p
;;                    q
;;                    (- count 1)))))

;; excercise 1.20

;; applicative order

;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd 40 6)
;; (gcd 6 (remainder 40 6))
;; (gcd 6 4)
;; (gcd 4 (remainder 6 4))
;; (gcd 2 2)
;; (gcd 2 (remainder 2 2))
;; (gcd 2 0)
;; 2

;; ;; normal order

;; (gcd 206 40)
;; (gcd (if (= 40 0) 206 (gcd 40 (remainder 206 40))))

;; (gcd 40 (remainder 206 40))
;; ...

;; excercise 1.21
(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor
               n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

;; excercise 1.22

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime)
                       start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes n m)
  (cond ((< n m) (timed-prime-test n) (search-for-primes (inc n) m))))

;;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))


;; expmod 2 8 6 = square (expmod 2 4 6) mod 6 = square 4 mod 6 = 16 mod 4 = 4

;; expmod 2 4 6 = square (expmod 2 2 6) mod 6 = square 4 mod 6 = 16 mod 6 = 4

;; expmod 2 2 6 = square (expmod 2 1 6) mod 6 = square 2 mod 6 = 4 mod 6 = 4

;; expmod 2 1 6 = 2 * (expmod 2 0 6) mod 6 = 2 * 1 mod 6 = 2 mod 6 = 2

;; expmod 2 0 6 = 1

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
         (else false)))

;; 1.3.1

(define (cube x) (* x x x))

;; (define (sum-integers a b)
;;   (if (> a b)
;;       0
;;       (+ a (sum-integers (+ a 1) b))))

;; (define (sum-cubes a b)
;;   (if (> a b)
;;       0
;;       (+ (cube a)
;;          (sum-cubes (+ a 1) b))))
;; (define (pi-sum a b)
;;   (if (> a b)
;;       0
;;       (+ (/ 1.0 (* a (+ a 2)))
;;          (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-integral f a b n)
  (define (h) (/ (- b a) n))
  (define (y k) (f (+ a (* k (h)))))
  (define (term k)
    (cond ((= k 0) (y k))
          ((= k n) (y k))
          ((odd? k) (* 4 (y k)))
          (else (* 2 (y k)))))
  (* (/ (h) 3.0)
     (sum term 0 inc n)))

;; excercise 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


;; excercise 1.31

(define (product f a next b)
  (if (> a b)
      1.0
      (* (f a) (product f (next a) next b))))

(define (factorial-product n)
  (product identity 1 inc n))

(define (pi-williams n)
  (define (next x) (+ x 2))
  (define (term a)
    (/ (* a (+ a 2)) (* (+ a 1) (+ a 1))))
  (* 4 (product term 2 next n)))

(define (product-iter f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (f a) result))))
  (iter a 1))

;; excercise 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

;; (accumulate + 0 identity 1 inc 5)
;; (accumulate * 1 identity 1 inc 5)

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter null-value a))

(define (filtered-accumulate-iter combiner null-value term a next b pred?)
  (define (iter a result)
    (if (> a b)
        result
        (if (pred? a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter null-value a))

;; (filtered-accumulate-iter + 0 square 1 inc 5 prime?)

(define (gcd a b)
  (cond ((= a 0) b)
        ((= b 0) a)
        (else (gcd b (remainder a b)))))

(define (relative-prime-product n)
  (define (relative-prime? x)
    (= 1 (gcd x n)))
  (filtered-accumulate-iter * 1 identity 1 inc n relative-prime?))

