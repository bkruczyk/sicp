#lang sicp

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-bindings exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (named-let? exp)
      (cadddr exp)
      (caddr exp)))

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

(define (named-let? exp)
  (not (pair? (cadr exp))))

(define (let-var exp)
  (cadr exp))

(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination exp)
      (cons (make-lambda
              (bindings->params (let-bindings exp))
              (list (let-body exp)))
             (bindings->args (let-bindings exp)))))

(define (named-let->combination exp)
  (cons
   (list
    (make-lambda '(f) '((f f)))
    (make-lambda (list (let-var exp))
                 (list (make-lambda (bindings->params (let-bindings exp))
                                    (list
                                     (list (make-lambda (list (let-var exp))
                                                        (list (let-body exp)))
                                           (list (let-var exp) (let-var exp))))))))
   (bindings->args (let-bindings exp))))

; (let->combination '(let ! ((x 5)) (if (< x 2) 1 (* (! (- x 1)) x))))

(((lambda (f) (f f))
  (lambda (!)
    (lambda (x)
      ((lambda (!)
         (if (< x 2)
             1
             (* (! (- x 1)) x))) (! !))))) 5)

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; doseq from clojure
(define (doseq? exp) (tagged-list? exp 'doseq))
(define (doseq->let exp)
  (list 'let 'doseq (list (cadr exp))
        (list 'if (list 'pair? (car (bindings->params (list (cadr exp)))))
              (sequence->exp (list
                              (list
                               (make-lambda (bindings->params (list (cadr exp)))
                                            (list (let-body exp)))
                               (list 'car (car (bindings->params (list (cadr exp))))))
                              (list 'doseq (list 'cdr (car (bindings->params (list (cadr exp)))))))))))

(let->combination (doseq->let '(doseq (x '(1 2 3 4 5)) (display x))))

(((lambda (f) (f f)) (lambda (doseq) (lambda (x) ((lambda (doseq) (if (pair? x) (begin ((lambda (x) (display x)) (car x)) (doseq (cdr x))))) (doseq doseq))))) '(1 2 3 4 5))
