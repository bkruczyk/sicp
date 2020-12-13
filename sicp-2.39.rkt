#lang sicp

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequance)
  (if (null? sequance)
      initial
      (op (car sequance)
          (fold-right op initial (cdr sequance)))))

(define (reverse-r sequence)
  (fold-right
   (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-l sequence)
  (fold-left
   (lambda (x y) (cons y x)) nil sequence))
