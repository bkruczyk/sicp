#lang sicp

(define (square x) (* x x))

(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (tree-map f (car tree)) (tree-map f (cdr tree))))
        (else (f tree))))

(define (square-tree tree)
  (tree-map square tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
