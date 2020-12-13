#lang sicp

;; excercise 2.73
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

;; 1) number and variable predicates works on atomic things so they don't fit
;; in operator/operend interface

;; 2, 3)

(define (make-entry op type item)
  (list op type item))
(define (entry-op entry)
  (car entry))
(define (entry-type entry)
  (cadr entry))
(define (entry-item entry)
  (caddr entry))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base p)
  (cadr p))
(define (exponent p)
  (caddr p))

(define (** base exponent)
  (cond ((= exponent 0) 1)
        ((= base 1) 1)
        (else (* base (** base (- exponent 1))))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        ((and (number? base) (number? exponent)) (** base exponent))
        (else (list '** base exponent))))

(define dispatch-table
  (list (make-entry '+ 'deriv (lambda (operands var)
                                (let ((exp (cons '+ operands)))
                                  (make-sum (deriv (addend exp) var)
                                            (deriv (augend exp) var)))))
        (make-entry '* 'deriv make-product)
        (make-entry '** 'deriv make-exponentiation)))

(define (get type op)
  (define (get-from-table op type entries)
    (cond ((null? entries)
           (error "MISSING OP TYPE" op type))
          ((and (eq? op (entry-op (car entries)))
                (eq? type (entry-type (car entries))))
           (entry-item (car entries)))
          (else (get-from-table op type (cdr entries)))))
  (get-from-table op type dispatch-table))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp)
           (if (same-variable? exp var)
               1
               0))
         (else ((get 'deriv (operator exp))
                (operands exp)
                var))))

;; 4) it is enough to change get method signature


;; excercise 2.74
;;
;; I used simple table with predefined mappings, otherwise it would just require
;; to install-package for department in insatible-data-table

;; 1, 2, 3, 4)

;; Each division must implement its own get-record and employee structure.
;;
;; Assumption is made that name of the employee is the index element and its
;; identity. However other method could be used as employee identity, given each
;; type of employee would supply identity method. Identity implies there can be
;; only on employee with given name in personel file.
;;
;; To support get-salary record does not have to be structured in particular way
;; as long as its package expose method for getting salary from employee entity
;;
;; When Insatiable takes over new company then it has to register mapping of
;; their internal employee manipulation procedure into insatiable-data-table.

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum:
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum:
              CONTENTS" datum)))

(define (acme/name employee)
  (car employee))
(define (acme/salary employee)
  (cadr employee))

(define (acme/get-record employee personel-file)
  (cond ((null? personel-file) nil)
        ((= (acme/name employee) (acme/name (car personel-file)))
         (car (personel-file)))
        (else (acme/get-record employee (cdr personel-file)))))

;; contains data mappings for acme department, could be added by install-package
(define insatible-data-table
  (list
   (make-entry 'acme 'get-record
               (lambda (employee-name personel-file)
                 (attach-tag 'acme (acme/get-record employee-name personel-file))))
   (make-entry 'acme 'get-salary acme/salary)))

(define (insatible/get type op)
  (define (get-from-table op type entries)
    (cond ((null? entries)
           (error "MISSING OP TYPE" op type))
          ((and (eq? op (entry-op (car entries)))
                (eq? type (entry-type (car entries))))
           (entry-item (car entries)))
          (else (get-from-table op type (cdr entries)))))
  (get-from-table op type insatible-data-table))

(define (get-record employee-name personel-file)
  ((insatible/get (type-tag personel-file) 'get-record)
   employee-name
   (contents personel-file)))

(define (get-salary employee)
  ((insatible/get (type-tag employee) 'get-salary) employee))

(define (find-employee-record employee-name personel-files)
  (cond ((null? personel-files) '())
        (else
         (cons (get-record employee-name (car personel-files))
               (find-employee-record employee-name (cdr personel-files))))))

;; excercise 2.75

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* mag (cos ang)))
          ((eq? op 'imag-part)
           (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else (error "no such op MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; excercise 2.76

;; With explicit dispatch: each time new type is created we must handle it in
;; ALL methods, each time new method in created we mast handle ALL existing
;; types

;; With data directed style when ONE new type is created it must be registered in
;; dispatch table, when new operation is added we must add it in ALL
;; packages,

;; With message passing style when new type is added we need add ONE constructor
;; with dispatch method, when new operation is added we need to update our
;; dispatch method in ALL types.

;; In regards to additivity
;;
;; using message-passsing style we can additively add new types
;; with their method implementations, adding new method to "interface" would
;; require altering exsiting types
;;
;; using explicit apprach we can additively add new generic methods with
;; handling for existing types, but when we want to add new type then we need to
;; alter all existing methods
;;
;; using data-directed style we can additivaly add types or methods by
;; registering them in dispatch table
