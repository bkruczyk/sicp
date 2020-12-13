#lang sicp

;; excercise 1.29

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

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
