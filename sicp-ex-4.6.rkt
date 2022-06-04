#lang sicp

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (let?)
  (tagged-list? exp 'let))

(define (let-bindings exp)
  (cadr exp))
(define (let-body exp)
  (caddr exp))

(define (bindings->params bindings)
  (if (null? bindings)
      bindings
      (cons
       (caar bindings)
       (bindings->params (cdr bindings)))))

(define (bindings->args bindings)
  (if (null? bindings)
      bindings
      (cons
       (cadar bindings)
       (bindings->args (cdr bindings)))))

;; TODO Check if bindings are pairs
(define (let->combination exp)
  (cons
   (make-lambda
    (bindings->params (let-bindings exp))
    (list (let-body exp)))
   (bindings->args (let-bindings exp))))

;; (let->combination '(let ((a "1") (b "2")) (+ a b)))
