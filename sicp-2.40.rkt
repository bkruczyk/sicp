#lang sicp

(define (square x) (* x x))

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

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init
                        (accumulate
                         (lambda (x y) (cons (car x) y))
                         nil
                         seqs))
            (accumulate-n op init
                          (accumulate
                           (lambda (x y) (cons (cdr x) y))
                           nil
                           seqs)))))

(define (enumerate-interval x y)
  (if (> x y)
      nil
      (cons x (enumerate-interval (+ 1 x) y))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (filter f seq)
  (cond ((null? seq) nil)
        ((f (car seq)) (cons (car seq) (filter f (cdr seq))))
        (else (filter f (cdr seq)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (unique-pairs n))))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permutations
                       (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

;; 2.40
(define (unique-pairs n)
  (flatmap (lambda (x)
         (map (lambda (y) (list x y))
              (enumerate-interval x n)))
       (enumerate-interval 1 n)))

;; 2.41
(define (make-triples n)
  (flatmap (lambda (x)
         (flatmap (lambda (y)
                (map (lambda (z)
                       (list x y z))
                     (enumerate-interval y n)))
              (enumerate-interval x n)))
       (enumerate-interval 1 n)))

(define (find-triples n s)
  (filter (lambda (triple) (= s (accumulate + 0 triple)))
          (make-triples (+ n 1))))

;; 2.42
(define empty-board nil)

(define (k-row k positions)
  (let ((row (filter (lambda (x)
                             (= k (car x)))
                           positions)))
    (if (null? row)
        nil
        (cadr (car row)))))

;; need to include diagonals
(define (safe? k positions)
  (let ((row (k-row k positions)))
    (null? (filter (lambda (x)
                     (or
                      (and
                       (= (cadr x) row)
                       (not (= (car x) k)))
                      (is-diagonal-with (list k row) x)))
                   positions))))

(define (diagonal-up-left x n)
  (list (- (car x) n) (- (cadr x) n)))
(define (diagonal-down-left x n)
  (list (- (car x) n) (+ (cadr x) n)))

(define (same? x y)
  (and (= (car x) (car y))
       (= (cadr x) (cadr y))))

(define (recur a b i)
  (if (= 0 (- (car a) i))
      #f
      (or (same? b (diagonal-down-left a i))
          (same? b (diagonal-up-left a i))
          (recur a b (+ 1 i)))))
(define (is-diagonal-with a b)
  (recur a b 1))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list k new-row) rest-of-queens))

(#%require racket/trace)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; 2.43
;; recursive queen-cols call is repeated board-size times
;; if 2.42 solve the puzzle in time T then 2.43 will solve
;; it in T^2 -- on each column k queen-colls is called board-size
;; times so (n - 1) x n ~ n^2
