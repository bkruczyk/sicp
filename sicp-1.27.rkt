#lang sicp

(define (square n) (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor
               n (next test-divisor)))))

(define (prime? n)
  (= n (smallest-divisor n)))

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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (congruent-exp? a n)
  (= (expmod a n n) a))

(define (carmichael? n)
  (define (recur a n)
    (cond ((= a n) #t)
          ((congruent-exp? a n) (recur (inc a) n))
          (else #f)))
  (recur 0 n))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
         (else false)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))


(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime n (- (runtime)
                       start-time))))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (search-for-primes n m)
  (cond ((< n m) (timed-prime-test n) (search-for-primes (inc n) m))))
