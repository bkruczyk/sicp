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

;; op musi być symetryczna
