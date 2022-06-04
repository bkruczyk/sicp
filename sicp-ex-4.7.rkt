#lang sicp

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (let-bindings exp)
  (cadr exp))
(define (let-body exp)
  (cddr exp))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (make-let bindings body)
  (list 'let bindings body))

(define (let*->nested-lets exp)
  (expand-lets (let-bindings exp)
               (car (let-body exp))))

(define (expand-lets bindings body)
  (if (null? bindings)
      body
      (make-let
       (list (car bindings))
       (expand-lets (cdr bindings) body))))

;; (let*->nested-lets '(let* ((a 1) (b (+ a 1))) (+ a b)))

;; It is sufficient to to expand let* to nested lets since each let will be
;; expanded to lambda and each lambda is evaluated to procedure that have inside
;; another let etc.
