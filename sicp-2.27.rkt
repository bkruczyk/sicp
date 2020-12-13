#lang sicp

(define x
  (list (list 1 2) (list 3 4)))

(define (deep-reverse items)
  (cond
    ((null? items) items) ;; ()
    ((and (pair? (car items)) (null? (cdr items))) ;; ((1 2))
     (list (deep-reverse (car items))))
    ((and (pair? (car items)) (pair? (car (cdr items)))) ;; ((1 2) (3 4))
     (list (deep-reverse (car (cdr items))) (deep-reverse (car items))))
    ((pair? (car items))
     (list (car (cdr items)) (deep-reverse (car items)))) ;; ((1 2) 3)
    ((null? (cdr items))  ;; (1)
     (list (car items)))
    ((pair? (car (cdr items)))
     (list (deep-reverse (car (cdr items))) (car items))) ;; (1 (2 3))
    (else (append (deep-reverse (cdr items)) (list (car items)))) ;; (1 2)
    ))
