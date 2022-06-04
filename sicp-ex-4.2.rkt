;; Exercise 4.2

;; 1. Implementation of `application?` procedure is done so that it only checks
;; if expression is pair. Evaluator will try to apply procedure `define` which
;; would result in evaluator trying to evalute assignment variable `x` when it
;; should be treated as a symbol.

;; 2.

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
