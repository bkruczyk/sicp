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

(define (make-account balance secret)
  (define (withdraw password amount)
    (if (eq? password secret)
        (if (>= balance amount)
            (begin (set! balance
                         (- balance amount))
                   balance)
            "Insufficient funds")
        "Incorrect password"))
  (define (deposit password amount)
    (if (eq? password secret)
        (begin
          (set! balance (+ balance amount))
          balance)
        ("Incorrect password")))
  (define (dispatch p m)
    (cond ((eq? m 'withdraw) (lambda (x) (withdraw p x)))
          ((eq? m 'deposit) (lambda (x) (deposit p x)))
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 100 'secret-password))

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
