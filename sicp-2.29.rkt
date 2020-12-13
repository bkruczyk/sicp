#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (left-branch mobile)
  (car mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define x (make-mobile (make-branch 1 5) (make-branch 1 (make-mobile (make-branch 1 2) (make-branch 1 3)))))

(define (branch-weight branch)
  (let ((value (branch-structure branch)))
    (cond ((pair? value) (total-weight value))
          (else value))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (branch-torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (leaf? branch)
  (not (pair? (branch-structure branch))))

(define (balanced? mobile)
  (let ((lb (left-branch mobile))
        (rb (right-branch mobile)))
    (cond ((and (leaf? lb) (leaf? rb))
           (= (branch-torque lb) (branch-torque rb)))
          ((and (leaf? lb) (not (leaf? rb)))
           (and (= (branch-torque lb) (branch-torque rb))
                (balanced? (branch-structure rb))))
          ((and (not (leaf? lb)) (leaf? rb))
           (and
            (= (branch-torque lb) (branch-torque rb))
            (balanced? (branch-structure lb))))
          (else
           (and
            (= (branch-torque lb) (branch-torque rb))
            (balanced? (branch-structure lb))
            (balanced? (branch-structure rb)))))))
