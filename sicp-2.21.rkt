#lang sicp

(define (square x) (* x x))

(define (square-list-a items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-a (cdr items)))))

(define (square-list-b items)
  (map square items))
