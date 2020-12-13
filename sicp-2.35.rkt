#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (count-leaves t)
  (accumulate
   (lambda (x y) (+ x y))
   0
   (map (lambda (x)
          (if (pair? x)
              (count-leaves x)
              1))
        t)))
