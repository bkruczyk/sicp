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
  (define (contains? xs x)
    (cond ((null? xs) false)
          ((eq? (car xs) x) true)
          (else (contains? (cdr xs) x))))
  (let ((max-wrong-passwords 7)
        (wrong-passwords-given 0)
        (passwords (list secret)))
    (define (add-new-password new-password)
      (set! passwords (cons new-password passwords)))
    (define (with-password-check password f . args)
      (if (contains? passwords secret)
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
            ((eq? m 'deposit) (lambda (x) (with-password-check p deposit x)))
            ((eq? m 'add-new-password) (lambda (x) (with-password-check p add-new-password x)))
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define acc (make-account 100 'secret-password))

;; excercise 3.1

(define (make-accumulator n)
  (lambda (x)
    (begin
      (set! n (+ n x))
      n)))

;; (define A (make-accumulator 5))

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

;; (define rand
;;   (let ((x random-init))
;;     (lambda ()
;;       (set! x (rand-update x))
;;       x)))

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

;; excercise 3.6
(define rand
  (let ((x random-init))
    (lambda (op)
      (cond ((eq? op 'generate)
             (begin
               (set! x (rand-update x))
               x))
            ((eq? op 'reset)
             (lambda (y)
               (set! x y)))))))

;; excercise 3.7
;; modification to make-account done above
(define (make-joint account password new-password)
  ((account password 'add-new-password) new-password)
  account)

(define peter-acc (make-account 100 'foo))
(define paul-acc (make-joint peter-acc 'foo 'bar))

;; excercise 3.8
(define f
  (let ((y 1))
    (lambda (x)
      (set! y (* y x))
      y)))

;; when executed left-to-right will give 0 because we will
;; first multiply y by 0 and we will multiply 1 * 0 in next
;; evaluation
;;
;; right-to-left we will start with 1 * 1 and after that we
;; will add 1 * 0 to it so it will give 1 as the sum

;; (+ (f 0) (f 1))
;;
;;

;; excercise 3.9
;; excercise 3.10
;;
;; pen and paper tasks
;; http://community.schemewiki.org/?sicp-ex-3.10

;; excercise 3.11
;; acc state is kept in the first frame created (E0)
;; different accounts have separate environments
;; code for them can be shared
;; http://community.schemewiki.org/?sicp-ex-3.11

;; excercise 3.12

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;; (define x (list 'a 'b))
;; (define y (list 'c 'd))
;; (define z (append x y))

;; (define w (append! x y))

;; (cdr x)
;;
;; append! is modifying x by appending y, w is here just other name for x

;; excercise 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
;; (define z (make-cycle (list 'a 'b 'c)))

;; make-cycle creates, indeed, a cycle so that the successor of 'c is 'a
;; trying to compute last-pair on z will cause infinite loop

;; excercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

;; note for the SICP paragraph
;; eq? is comparing using pointers, not values
;; meaning comparing lists
;; (define a '(1 2 3))
;; (define b '(1 2 3))
;; will yield false
;; but when we compare symbols eg.
;; (eq? 'a 'a)
;; it will yield true since symbols are unique
;; in Scheme meaning they share the same space
;; and they have the same pointer

;; excercise 3.15
;; drawing excercise

;; excercise 3.16

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; would not return for any list containing a cycle eg.
;; (define x '(a b c))
;; (set-car! x x)
;; (count-pairs x)

;; would return 3 for simple three-element list
;; (count-pairs '(a b c))

;; would return 4 because it would count twice pair that contains b
;;
;; (define p3 (cons 'b nil))
;; (define p2 (cons 'a nil))
;; (define p1 (cons nil nil))
;; (set-cdr! p2 p3)
;; (set-car! p1 p2)
;; (set-cdr! p1 p3)
;; (count-pairs p1)
;; => 4
;;
;; dont use display to check structure of p1!
;; it does not show the refernces, it shows as if values were copied
;;
;; -> | * | * | ------ | b | / |
;;      |                |
;;      |                |
;;    | a | * | --------/
;;
;; return 7, traversing to values a b once from car of first pair,
;; and second pair while traversing from cdr
;;
;; -> | * | * |
;;      |  /
;;      | /
;;    | * | * |
;;      |  /
;;      | /
;;    | * | * |
;;      |   |
;;      a   b
;;
;; excercise 3.17
(define (member? x xs)
  (cond ((null? xs)
         #f)
        ((eq? x (car xs))
         #t)
        (else
         (member? x (cdr xs)))))

;; (define p3 (cons 'b nil))
;; (define p2 (cons 'a nil))
;; (define p1 (cons nil nil))
;; (set-cdr! p2 p3)
;; (set-car! p1 p2)
;; (set-cdr! p1 p3)
;; (count-pairs p1)

(define (count-pairs-bis xs)
  (define visited '())
  (define (loop xs)
    (if (not (pair? xs))
        0
        (let ((head (car xs))
              (tail (cdr xs)))
          (+ 1
             (if (member? head visited)
                 0
                 (begin
                   (set! visited (cons head visited))
                   (loop head)))
             (if (member? tail visited)
                 0
                 (begin
                   (set! visited (cons tail visited))
                   (loop tail)))))))
  (loop xs))

;; (count-pairs-bis p1)
;; => 3

;; excercise 3.18

(define (has-cycle? xs)
  (define visited '())
  (define (loop xs)
    (cond ((not (pair? xs))
           #f)
          ((member? (cdr xs) visited)
           #t)
          (else
           (set! visited (cons (cdr xs) visited))
           (loop (cdr xs)))))
  (loop xs))

;; excercise 3.19
;; This one is not *clever* although it is constant space.
;; Visited contains alwyas at most single element.
;; But the time complexity is quadratic n^2.
;;
;; There are algorithms that use linear time, while using constant space,
;; eg. Floyd's rabbit and haare algorithm
;; https://en.wikipedia.org/wiki/Cycle_detection

(define (has-cycle-cons-space? xs)
  (define visited nil)
  (define (seek xs)
    (cond ((null? xs)
           #f)
          ((eq? (car xs) visited)
           #t)
          (else
           (seek (cdr xs)))))
  (define (loop xs)
    (cond ((null? xs) #f)
          (else
           (set! visited (car xs))
           (if (seek (cdr xs))
               #t
               (loop (cdr xs))))))
  (loop xs))

;; excercise 3.20
;; environment box drawing excercise

;; excercise 3.21
;; Ben's examples are printing queue representation which
;; is just a cons of front and rear pointers.
;; While rear pointer is always cons of (value . nil),
;; the front pointer is displayed as the list that starts
;; where front pointer is pointing to.

(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with and empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue)
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue"))
        (else (set-front-ptr!
               queue
               (cdr (front-ptr queue)))
              queue)))

(define (print-queue queue)
  (display (front-ptr queue)))

;; excercise 3.22
(define (make-queue!)
  (let ((front-ptr nil)
        (rear-ptr nil))
    (define (empty-queue?)
      (null? front-ptr))
    (define (insert! x)
      (cond ((empty-queue?)
             (let ((cell (cons x nil)))
               (set! front-ptr cell)
               (set! rear-ptr cell)))
            (else
             (set-cdr! rear-ptr (cons x nil))
             (set! rear-ptr (cdr rear-ptr)))))
    (define (delete!)
      (if (empty-queue?)
          (error "DELETE! From empty queue")
          (set! front-ptr (cdr front-ptr))))
    (define (dispatch m)
      (cond ((eq? m 'insert) (lambda (x) (insert! x)))
            ((eq? m 'delete) (delete!))
            ((eq? m 'print) (display front-ptr))
            (else
             (error "DISPATCH! Operation not supported" m))))
    dispatch))

;; excercise 3.22
(define (make-dequeue)
  (cons '() '(nil nil)))

(define (empty-dequeue? dequeue)
  (null? (car dequeue)))

(define (front-insert-dequeue! dequeue x)
  (let ((new-pair (cons x (car dequeue))))
    (cond ((empty-dequeue? dequeue)
           (set-car! dequeue new-pair)
           (set-cdr! dequeue (list new-pair)))
          (else
           (set-car! dequeue new-pair)))))

(define (rear-insert-dequeue! dequeue x)
  (let ((new-pair (cons x nil)))
    (cond ((empty-dequeue? dequeue)
           (set-car! dequeue new-pair)
           (set-cdr! dequeue (list new-pair)))
          (else
             ;; (cdr dequeue) -- rear ptr
             ;; (car (cdr dequeue)) -- last pair
             ;; (cdr (cdr dequeue)) -- previous pair
           (let ((rear-ptr (cdr dequeue))
                 (last-pair (car (cdr dequeue))))
             (set-cdr! last-pair new-pair)
             (set-cdr! dequeue (cons new-pair rear-ptr)))))))

(define (front-delete-dequeue! dequeue)
  (if (null? (car dequeue))
      (error "DELETE FRONT! Empty dequeue")
      (set-car! dequeue (cdr (car dequeue)))))
(define (rear-delete-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
         (error "DELETE REAR! Empty dequeue"))
        ((null? (cdr (cdr dequeue)))
         (set-car! dequeue nil)
         (set-cdr! dequeue (cons nil nil)))
        (else
         (set-cdr! dequeue (cdr (cdr dequeue)))
         (set-cdr! (car (cdr dequeue)) nil))))

;; (define d (make-dequeue))
;; (front-insert-dequeue! d 'x)
;; (front-insert-dequeue! d 'y)
;; (rear-insert-dequeue! d 'a)
;; (rear-insert-dequeue! d 'b)
;; (rear-insert-dequeue! d 'c)
;; (display d)
;; (newline)
;; (rear-delete-dequeue! d)
;; (display d)
;; (newline)
;; (rear-delete-dequeue! d)
;; (display d)
;; (newline)
;; (rear-delete-dequeue! d)
;; (display d)
;; (newline)
;;
;; -- end of excercise 3.23

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (car (car records)))
         (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value)
                              (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;; (define (lookup key-1 key-2 table)
;;   (let ((subtable (assoc key-1 (cdr table))))
;;     (if subtable
;;         (let ((record (assoc key-2 (cdr subtable))))
;;           (if record
;;               (cdr record)
;;               false)))))

;; (define (insert! key-1 key-2 value table)
;;   (let ((subtable (assoc key-1 (cdr table))))
;;     (if subtable
;;         (let ((record
;;                (assoc key-2 (cdr subtable))))
;;           (if record
;;               (set-cdr! record value)
;;               (set-cdr!
;;                subtable
;;                (cons (cons key-2 value)
;;                      (cdr subtable)))))
;;         (set-cdr!
;;          table
;;          (cons (list key-1 (cons key-2 value))
;;                (cdr table)))))
;;   'ok)

;; excercise 3.24

;; (define (make-table)
;;   (let ((local-table (list '*table*)))
;;     (define (lookup key-1 key-2)
;;       (let ((subtable (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record (assoc key-2 (cdr subtable))))
;;               (if record
;;                   (cdr record)
;;                   false))
;;             false)))
;;     (define (insert! key-1 key-2 value)
;;       (let ((subtable (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record (assoc key-2 (cdr subtable))))
;;               (if record
;;                   (set-cdr! record value)
;;                   (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
;;             (set-cdr! local-table
;;                       (cons (list key-1 (cons key-2 value))
;;                             (cdr local-table)))))
;;       'ok)
;;     (define (dispatch m)
;;       (cond ((eq? m 'lookup-proc) lookup)
;;             ((eq? m 'insert-proc!) insert!)
;;             (else (error "Unknown operation: TABLE" m))))
;;     dispatch))

;; (define operation-table (make-table))
;; (define get (operation-table 'lookup-proc))
;; (define put (operation-table 'insert-proc!))

;; (define (make-table same-key?)
;;   (let ((local-table (list '*table*)))
;;     (define (assoc key records)
;;       (cond ((null? records) false)
;;             ((same-key? key (car (car records)))
;;              (car records))
;;             (else (assoc key (cdr records)))))
;;     (define (lookup key-1 key-2)
;;       (let ((subtable (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record (assoc key-2 (cdr subtable))))
;;               (if record
;;                   (cdr record)
;;                   false))
;;             false)))
;;     (define (insert! key-1 key-2 value)
;;       (let ((subtable (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record (assoc key-2 (cdr subtable))))
;;               (if record
;;                   (set-cdr! record value)
;;                   (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
;;             (set-cdr! local-table
;;                       (cons (list key-1 (cons key-2 value))
;;                             (cdr local-table)))))
;;       'ok)
;;     (define (dispatch m)
;;       (cond ((eq? m 'lookup-proc) lookup)
;;             ((eq? m 'insert-proc!) insert!)
;;             (else (error "Unknown operation: TABLE" m))))
;;     dispatch))

;; (define operation-table (make-table))
;; (define get (operation-table 'lookup-proc))
;; (define put (operation-table 'insert-proc!))

;; excercise 3.25
;; (define (make-table)
;;   (define (assoc key records)
;;     (display "assoc: key=")
;;     (display key)
;;     (display ", records=")
;;     (display records)
;;     (newline)
;;     (cond ((null? records) false)
;;           ((equal? key (car (car records)))
;;            (car records))
;;           (else (assoc key (cdr records)))))
;;   (let ((local-table (list '*table*)))
;;     (define (lookup keys subtable)
;;       (display "lookup: keys=")
;;       (display keys)
;;       (display ", subtable=")
;;       (display subtable)
;;       (newline)
;;       (cond ((null? subtable) false)
;;             ((null? keys) false)
;;             ((null? (cdr keys))
;;              (display "Looking up for record...")
;;              (newline)
;;              (let ((record (assoc (car keys) (cdr subtable))))
;;                (display "(car keys): ")
;;                (display (car keys))
;;                (newline)
;;                (display "(cdr subtable): ")
;;                (display (cdr subtable))
;;                (newline)
;;                (display "record: ")
;;                (display record)
;;                (newline)
;;                (if record
;;                    (cdr record)
;;                    false)))
;;             (else
;;              (lookup (cdr keys) (cdr subtable)))))
;;     (define (insert! keys subtable value)
;;       (cond ((null? (cdr keys))
;;              (let ((record (assoc (car keys) (cdr subtable))))
;;                (if record
;;                    (set-cdr! record value)
;;                    (set-cdr! subtable (cons (cons (car keys) value) (cdr subtable))))))
;;             ((null? (cdr subtable))
;;              (set-cdr! subtable (cons (cons (car keys) nil) nil))
;;              (insert! (cdr keys) (cdr subtable) value))
;;             (else
;;              (insert! (cdr keys) (cdr subtable) value)))
;;       'ok)
;;     (define (dispatch m)
;;       (cond ((eq? m 'lookup-proc) (lambda (keys) (lookup keys local-table)))
;;             ((eq? m 'insert-proc!) (lambda (keys value) (insert! keys local-table value)))
;;             (else (error "Unknown operation: TABLE" m))))
;;     dispatch))

;; (define operation-table (make-table))
;; (define get (operation-table 'lookup-proc))
;; (define put (operation-table 'insert-proc!))

;; excercise 3.26
;; It could be done using binary tree so that when new keys are inserted
;; the binary tree gets rebuilt.
;
;; Alternatively we can keep current implementation but at the same time
;; maintain additional index structure which would be rebuilt on inserting
;; new keys.

;; excercise 3.27

;; memo-fib is linear because it computes given n once, for the n elements it
;; has to do n times summation, which we assume is O(1) and 2n table accesses;
;; we can assume table access is O(1) since last computed values are added to
;; the top of the list so accessing them does not require going through whole
;; precomputed value list
;;
;; note:
;; in global environment thing that will be bound to 'memo-fib' is the lamda
;; returned by memoize, so after calling memo-fib again it will execute memoized
;; version, and will not create again local-table

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else
            (display "Computing n=")
            (display n)
            (newline)
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2))))))))

;; TODO Do the excercises from the section about simulating digital circuits.

;; 3.3.5 Propagation of Constraints

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1)
                (has-value? a2))
           (set-value! sum
                       (+ (get-value a1)
                          (get-value a2))
                       me))
          ((and (has-value? a1)
                (has-value? sum))
           (set-value! a2
                       (- (get-value sum)
                          (get-value a1))
                       me))
          ((and (has-value? a2)
                (has-value? sum))
           (set-value! a1
                       (- (get-value sum)
                          (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Uknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1)
                    (= (get-value m1) 0))
               (and (has-value? m2)
                    (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1)
                (has-value? m2))
           (set-value! product
                       (* (get-value m1)
                          (get-value m2))
                       me))
          ((and (has-value? product)
                (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product)
                (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ")
    (display name) (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknow request: PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except
              setter
              inform-about-value
              constraints))
            ((not (= value newval)
                  (error "Contradiction"
                         (list value newval))))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except
                  retractor
                  inform-about-no-value
                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint
                     constraints))
          (set! constraints
                (cons new-constraint
                      constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!)
             set-my-value)
            ((eq? request 'forget)
             forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Uknown operation: CONNECTOR" request))))
    me))

(define (for-each-except exception
                         procedure
                         list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception)
           (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector
                    new-value
                    informant)
  ((connector 'set-value!)
   new-value
   informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;; (define (celsius-fahrenheit-converter c f)
;;   (let ((u (make-connector))
;;         (v (make-connector))
;;         (w (make-connector))
;;         (x (make-connector))
;;         (y (make-connector)))
;;     (multiplier c w u)
;;     (multiplier v x u)
;;     (adder v y f)
;;     (constant 9 w)
;;     (constant 5 x)
;;     (constant 32 y)
;;     'ok))

;; (define C (make-connector))
;; (define F (make-connector))
;; (celsius-fahrenheit-converter C F)

;; (probe "Celsius temp" C)
;; (probe "Fahrenheit temp" F)

;; (set-value! F 212 'user)
;; (forget-value! F 'user)
;; (forget-value! C 'user)
;; (set-value! C 100 'user)

;; excercise 3.33
(define (averager a b c)
  (define (process-new-value)
    (cond ((and (has-value? a)
                (has-value? b))
           (set-value! c
                       (* 1.0 (/ (+ (get-value a) (get-value b)) 2))
                       me))
          ((and (has-value? a)
                (has-value? c))
           (set-value! b
                       (- (* 2 (get-value c)) (get-value a))
                       me))
          ((and (has-value? b)
                (has-value? c))
           (set-value! a
                       (- (* 2 (get-value c)) (get-value b))
                       me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (forget-value! c me))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Uknown request: AVERAGER" request))))
  (connect a me)
  (connect b me)
  (connect c me)
  me)


;; (define X (make-connector))
;; (define Y (make-connector))
;; (define Z (make-connector))

;; (define averager-constrained (averager X Y Z))

;; (probe "Average" Z)
;; (set-value! X 2 'user)
;; (set-value! Y 3 'user)

;; excercise 3.34
;; squarer can't figure out the value of a if only b is set
;; this could be done if separate squarer contstraint would-be-future
;; be written that when b is given then it does sqrt
;; (define (squarer a b) (multiplier a a b))
;; (define A (make-connector))
;; (define B (make-connector))

;; (squarer A B)
;; (probe "Square" B)
;; (probe "A" A)
;; (set-value! B 4 'user)

;; excercise 3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER"
                   (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (* (get-value a) (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define A (make-connector))
(define B (make-connector))

(squarer A B)
;; (probe "Base" A)
;; (set-value! B 9 'user)
;; (probe "Square" B)
;; (set-value! A 2 'user)

;; excercise 3.36
;; TODO: Draw environment diagram

;; excercise 3.37
(define (c+ x y)
   (let ((z (make-connector)))
     (adder x y z)
     z))

 (define (c- x y)
   (let ((z (make-connector)))
     (adder z y x)
     z))

 (define (c* x y)
   (let ((z (make-connector)))
     (multiplier x y z)
     z))

 (define (c/ x y)
   (let ((z (make-connector)))
     (multiplier z y x)
     z))

 (define (cv x)
   (let ((z (make-connector)))
     (constant x z)
     z))

 (define (celsius-fahrenheit-converter x)
   (c+ (c* (c/ (cv 9) (cv 5))
           x)
       (cv 32)))
;; (define C (make-connector))
;; (define F (make-connector))
;; (celsius-fahrenheit-converter C F)

;; (probe "Celsius temp" C)
;; (probe "Fahrenheit temp" F)
