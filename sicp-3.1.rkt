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
  (define (call-the-cops) "COPS CALLED!")
  (let ((max-wrong-passwords 7)
        (wrong-passwords-given 0))
    (define (with-password-check password f . args)
      (if (eq? password secret)
          (apply f args)
          (if (>= wrong-passwords-given max-wrong-passwords)
              (call-the-cops)
              (begin
                (set! wrong-passwords-given (+ wrong-passwords-given 1))
                "Incorrect password"))))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Insufficient funds"))
    (define (deposit amount)
      (begin
        (set! balance (+ balance amount))
        balance))
    (define (dispatch p m)
      (cond ((eq? m 'withdraw) (lambda (x) (with-password-check p withdraw x)))
            ((eq? m 'deposit) (lambda (x) (with-password-check deposit p x)))
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

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

;; excercise 3.3
;; modification done above

;; excercise 3.4
;; modification done above
