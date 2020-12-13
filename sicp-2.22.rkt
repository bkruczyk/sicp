#lang sicp

(define (square x) (* x x))
;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons (square (car things))
;;                     answer))))
;;   (iter items nil))

;; newly squared numbers are added to the beginning of the list
;; so (cons 1 nil), (cons 2 (1 nil)) (cons 3 (2 1 nil)) etc
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square
                     (car things))))))
  (iter items nil))

;; for consing list must be 2-nd arg
