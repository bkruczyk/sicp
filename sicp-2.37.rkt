#lang sicp

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


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define v (list 2 2 2))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (map (lambda (y)
                  (dot-product x y))
                cols)) m)))
