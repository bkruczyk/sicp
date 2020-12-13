#lang sicp

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance
                       (- balance amount))
                 balance)
          "Insuffient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 100))

;; excercise 3.1

(define (make-accumulator n)
  (lambda (x)
    (begin
      (set! n (+ n x))
      n)))

(define A (make-accumulator 5))

;; excercise 3.2

(define (make-monitored f)
  (define (make-monitored-with-count f n)
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) n)
            ((eq? x 'reset-counter)
             (begin
               (set! n (- n n))
               n))
            (else
             (begin
               (set! n (+ 1 n))
               (f x))))))
  (make-monitored-with-count f 0))

(define s (make-monitored sqrt))
