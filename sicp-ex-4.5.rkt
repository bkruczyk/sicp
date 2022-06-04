#lang sicp

(define (make-if predicate
                 consequent
                 alternative)
  (list 'if
        predicate
        consequent
        alternative))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false    ;no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (if (cond-recipient-clause? first)
                         (list (cond-recipient first) (cond-predicate first)) ; cond-predicate will be executed twice, will not work if predicate is stateful
                         (sequence->exp
                          (cond-actions first)))
                     (expand-clauses
                      rest))))))

(define env (null-environment 5))

(define (cond-recipient-clause? clause)
  (eq? '=> (cadr clause)))
(define (cond-recipient clause)
  (caddr clause))

;; (cond->if '(cond ('(1 2) => car) (else "foo")))
