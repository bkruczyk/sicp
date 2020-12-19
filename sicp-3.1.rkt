#lang sicp

;;; 3.1.1 Local State Variables

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

;;; 3.1.2 The Benefits of Introducing Assignment
(define random-init 7901)

;; https://en.wikipedia.org/wiki/Middle-square_method
;; That's not very good method for random numbers, starting from 7901
;; I've end ep in cycle very quickly 4100 -> 8100 -> 6100 -> 2100 -> 4100
(define (middle-square x)
  (define (pad-zeroes str n)
    (if (= n (string-length str))
        str
        (pad-zeroes (string-append "0" str) n)))
  (let ((str (pad-zeroes (number->string x) 8)))
    (string->number (substring str 2 6))))

;; https://en.wikipedia.org/wiki/Lehmer_random_number_generator#Parameters_in_common_use
(define (minstd x)
  (let ((m 2147483647)
        (a 16807))
    (modulo (* a x) m)))
(define (rand-update x)
  (set! x (minstd x))
  x)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

;; excercise 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (point-in-integral P x1 x2 y1 y2)
  (lambda () (P (random-in-range x1 x2)
                (random-in-range y1 y2))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo
   trials
   (point-in-integral P x1 x2 y1 y2)))

(define (in-unit-circle? x y)
  (<= (+ (expt (- x 0.5) 2)
         (expt (- y 0.5) 2))
      (expt 0.5 2)))

(define (estimate-integral-pi)
  (let ((area
         (estimate-integral
          in-unit-circle?
          0.0 1.0 0.0 1.0
          100000.0)))
    (/ area (* 0.5 0.5))))
