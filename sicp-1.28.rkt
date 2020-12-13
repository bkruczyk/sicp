#lang sicp

(#%require racket/trace)

(define (square n) (* n n))

(define (expmod base exp m n)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder-or-nontrivial-square
          (square (expmod base (/ exp 2) m n))
          n
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m n))
          m))))

(define (remainder-or-nontrivial-square sq n m)
  (cond ((= sq 1) (remainder sq m))
        ((= sq (- n 1)) (remainder sq m))
        ((= sq (remainder 1 n)) 0)
        (else (remainder sq m))))

(define (miller-rabin-prime? n)
  (define (recur a n)
    (cond ((= a (- n 1)) #t)
          ((= (expmod a (- n 1) n n) 1) (recur (+ a 1) n))
          (else #f)))
  (recur 1 n))
