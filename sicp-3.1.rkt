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

;; excercise 3.38
;; 1)
;;
;; i)
;; 100 -> +10   = 110
;; 110 -> -20   = 90
;; 90  -> -HALF = 45
;;
;; ii)
;; 100 -> +10   = 110
;; 110 -> -HALF = 55
;; 55  -> -20   = 35
;;
;; iii)
;; 100 -> -20   = 80
;; 80  -> +10   = 90
;; 90  -> -HALF = 45
;;
;; iv)
;; 100 -> -20   = 80
;; 80  -> -HALF = 40
;; 40  -> +10   = 50
;;
;; v)
;; 100 -> -HALF = 50
;; 50  -> +10   = 60
;; 60  -> -20   = 40
;;
;; vi)
;; 100 -> HALF = 50
;; 50  -> -20  = 30
;; 30  -> +10  = 40
;;
;; 2)
;;
;;  Peter               Paul                Mary                Bank
;;
;;  access:    100      access:     100     access:    100      100
;;  new value: 110      new value:  80      new value: 50
;;  set! 110                                                    110
;;                      set! 80                                 80
;;                                          set! 50             50
;;
;;  Peter               Paul                Mary                Bank
;;
;;  access:    100      access:     100                         100
;;  new value: 110      new value:  80
;;  set! 110                                access: 110         110
;;                      set! 80             new value: 55       80
;;                                          set!                55
;;
;;  Peter               Paul                Mary                Bank
;;
;;  access:    100      access:     100                         100
;;  new value: 110      new value:  80
;;  set! 110                                                    110
;;                      set! 80             access: 80          80
;;                                          new value: 40
;;                                          set! 40             40
;;
;; etc.

;; excercise 3.39
;;
;; What serialization in Scheme means exactly? Depending on what
;; does it mean for a function to be serialized:
;;
;; 1) Serialized function can not be interleaved with other functions
;;
;; Posisible answers are exactly the same as if both P1 and P2 were seriazlized.
;; So either P1 starts first and completes, then P2 starts first and completes,
;; or P2 starts first and completes, then P1 starts first and completes.
;;
;; 2) Serialized functions that use the same serializer must run one after
;; another. If one function is using the serializer, but the other one does not,
;; then the result is the same as if both P1 and P2 weren't serialized.
;;
;; 3) Serialized function can not be affected y the value wrtite from other
;; function. Then still P1 can be interleaved by P2 and still all answers are
;; possible, as either
;;
;; - P1 runs and completes, then P2 runs and completes
;;
;; - P2 runs and completes, then P1 runs and completes
;;
;; - P1 starts running, read the initial value of x, the P2 starts and compltes,
;; then P1 writes its value
;;
;; - P2 starts running, P1 runs and completes, P2 completes
;;
;; - P1 starts running, read initial value of x, read first value of x in
;; multiplication, then P2 runs and completes, P1 reads second value of x in
;; multiplication, P2 completes
;;
;; NOTE: After reading the rest of the chapter it seems serialization is
;; basically making sure that functions that are using the same serializer in
;; one after another fashion. That would be that the case described in 2) is
;; true.
;;
;; excercise 3.40
;;
;; (define x 10)
;; (parallel-execute
;;  (lambda () (set! x (* x x)))
;;  (lambda () (set! x (* x x x)))
;;
;; 1)
;;
;; P1 10 * 10 = 100
;; P1 set! 100
;; P2 100 * 100 * 100 = 1_000_000
;; P2 set! 1_000_000
;;
;; 2)
;;
;; P2 10 * 10 * 10 = 1_000
;; P2 set! 1_000
;; P1 100 * 100 = 10_000
;; P1 set! 10_000
;;
;; 2b)
;;
;; P1 10
;; P2 10 * 10 * 10 = 1_000
;; P2 set! 1_000
;; P1 10 * 1000 = 10_000
;; P1 set! 10_000 ;; same result as previous
;;
;; 3)
;;
;; P1 10 * 10 = 100\
;; P2 10 * 10 * 10 = 1_000
;; P2 set! 1_000
;; P1 set! 100
;;
;; 4)
;;
;; P1 10 * 10 = 100
;; P2 10 *
;; P1 set! 100
;; P2 10 * 100 * 100 = 100_000
;; P2 set! 100_000
;;
;; 5)
;;
;; P1 10 * 10 = 100
;; P2 10 * 10 * 10 = 1_000
;; P1 set! 100
;; P2 set! 1_000
;;
;; excercise 3.41
;;
;; Access to balance does not to be serialized. During concurrent exectution
;; deposit and withdrawals can be interleaved with balance showing and it does
;; not do any anomalous behavior -- the state of balance is just the way it is
;; at given point of time, there is nothing more to it that can affect it during
;; execution, reading value of variable is atomic.
;;
;; excercise 3.42
;; http://community.schemewiki.org/?sicp-ex-3.42
;;
;; xdavidliu: Without further knowledge of how make-serial and parallel-execute
;; actually work, it's hard to tell whether a serialized function can be
;; interleaved *with itself*. If yes then for a modified version, there may be a
;; data race and the avoce block of code can incorrectly retyrn 90 or 80. If
;; not, then the modified version in this excercise works perfectly.
;;
;; A reasonable thing to say would be that since both arguments to
;; parallel-execute were serialized using the same serializer (since they are
;; actually the same *function*), we should expect them to not interleave with
;; each other, and hence the modification *should* work.
;;
;; excercise 3.43
;; TODO Drawing excercise
;;
;; excercise 3.44
;;
;; In exchange problem we need to stop-the-world state of two accounts balance
;; so that the diffenrence is still valid before going for the swap (because in
;; the meaintime value of balance acan change due to other exchange procedure).
;;
;; With transfer there is no such problem and no sophisticated synchronisation
;; is needed.
;;
;; excercise 3.45
;;
;; Exchange procedure will again wrap deposit and withdraw procedures with the
;; same serializers they are using. The nature of serializer is that at most one
;; procedure can run at the time for given serializer. This means that when
;; deposit or withdraw wrapped in exchange will start it will block the
;; underlying procedure that was serialized in make-account function.
;;
;; excercise 3.46
#lang sicp

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

;; excercise 3.46
;;
;; Both P1 and P2 acquire the mutex

;; P1 (car cell) -> false
;; P2 (car cell) -> false
;; P1 (set-car! cell true) -> false
;; P2 (set-car! cell true) -> false

;; excercise 3.47
;;
;; 1)
;;
;; (define (make-semaphore n)
;;   (let ((lock (make-mutex))
;;         (count n)))
;;   (define (the-semaphore m)
;;     (define (aquire)
;;       (lock 'acquire)
;;       (cond ((= n 0)
;;              (lock 'release)
;;              (aquire))
;;             (else
;;              (set! n (- n 1))
;;              (lock 'release))))
;;     (define (release)
;;       (lock 'release))
;;     (cond ((eq? m 'acquire) (aquire))
;;           ((eq? m 'release) (release)))))
;;
;; 2)
;;
;; (define (test-and-decrement! cell)
;;   (if (= cell 0)
;;       true
;;       (begin
;;         (set-car! cell (- (car cell) 1))
;;         false)))
;; (define (make-semaphore n)
;;   (let ((cell (list n)))
;;     (define (the-semaphore m)
;;       (cond ((eq? m 'acquire)
;;              (if (test-and-decrement! cell)
;;                  (the-semaphore 'acquire)))
;;             ((eq? m 'release) (clear! cell)))))
;;   the-semaphore))

;; excercise 3.48

;; (define (serialized-exchange account1 account2)
;;   (let ((serializer1 (account1 'serializer))
;;         (serializer2 (account2 'serializer)))
;;     ((serializer1 (serializer2 exchange))
;;      account1
;;      account2)))

;; Since accounts needs to be accessed always in the same order
;; regardless how they were passed to exchange function, the mutexes
;; will acquired in order, not allowing one procudere to get past the
;; point the deadlock can occur.
;;
;; (define (serialized-exchange account1 account2)
;;   (define (run account1 account2)
;;     (let ((serializer1 (account1 'serializer))
;;           (serializer2 (account2 'serializer)))
;;       ((serializer1 (serializer2 exchange))
;;        account1
;;        account2)))
;;   (if (< (account1 'id) (account2 'id))
;;       (run account1 account2)
;;       (run account2 account1)))))
;;
;; excercise 3.49
;;
;; The deadlock can occur perhaps where ordering can not be determined
;; or is nondeterministic or context-dependent. Eg. finding a next
;; state given a previous state.

;; 3.5 Streams

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; (define (stream-map proc s)
;;   (if (stream-null? s)
;;       the-empty-stream
;;       (cons-stream
;;        (proc (stream-car s))
;;        (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (prime? n)
  (define (loop x)
    (cond ((< n (* x x)) true)
          ((= 0 (modulo n x)) false)
          (else (loop (inc x)))))
  (cond ((= n 0) false)
        ((= n 1) false)
        (else (loop 2))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else  (filter predicate
                       (cdr sequence)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream
          (stream-car stream)
          (stream-filter
           pred
           (stream-cdr stream))))
        (else (stream-filter
               pred
               (stream-cdr stream)))))

;; excercise 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

;; excercise 3.51

;; (define (show x)
;;   (display-line x)
;;   x)

;; (define x
;;   (stream-map show (stream-enumerate-interval 0 10)))

;; (stream-ref x 5)

;; =>
;; 1
;; 2
;; 3
;; 4
;; 5

;; (stream-ref x 7)

;; =>
;; 6
;; 7

;; Expression (stream-ref x 7) evaluates the stream again but because
;; previous values are memoized, show function is not executed for
;; prior stream values.

;; excercise 3.52

;; (define sum 0)

;; (define (acum x)
;;   (set! sum (+ x sum))
;;   sum)

;; (display sum)
;; (newline)

;; (define seq
;;   (stream-map
;;    acum
;;    (stream-enumerate-interval 1 20)))

;; (display sum)
;; (newline)

;; (define y (stream-filter even? seq))

;; (display sum)
;; (newline)

;; (define z
;;   (stream-filter
;;    (lambda (x)
;;      (= (remainder x 5) 0)) seq))

;; (display sum)
;; (newline)

;; (stream-ref y 7) ;; => 136, sum => 210
;; (display-stream z)
;; =>
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210
;; sum => 210

;; If memoization have not been used then with each traverse of seq
;; the sum would get incremented.

;; 3.5.2 Infinite Streams

(define (integers-starting-from n)
  (cons-stream
   n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (take n s)
  "List of n first elements of the stream s.
   Useful for looking into the streams, not a part of SICP."
  (if (= n 0)
      nil
      (cons (stream-car s)
            (take (dec n) (stream-cdr s)))))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x)
                   (not (divisible? x 7)))
                 integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible?
                   x (stream-car stream))))
           (stream-cdr stream)))))

(define primes
  (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; (define integers
;;   (cons-stream 1 (add-streams ones integers)))

;; (define fibs
;;   (cons-stream
;;    0 (cons-stream
;;       1 (add-streams
;;          (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define double
  (cons-stream 1 (scale-stream double 2)))

;; Excercise 3.53

;; i    1  2  4 ...
;; i       =
;; ii      1  2 ...
;; i          =
;; iii        1 ...
;; i          +
;; iii        1 ...
;; i
;; i
;; i       +
;; ii      1  2 ...
;; i          =
;; iii        1 ...
;; i          +
;; iii        1 ...
(define s (cons-stream 1 (add-streams s s)))

;; Excercise 3.54

(define (mul-streams s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (mul-streams
                (stream-cdr s1)
                (stream-cdr s2))))

(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

;; Excercise 3.55

(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (add-streams (partial-sums s) (stream-cdr s))))

;; Excercise 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1
                          (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

;; Excercise 3.57

;; Thanks to memoization, there for nth Fibonacci number there are
;; required n additions. Without memoization stream would need to
;; compute its elements recursively. For nth number we need n-1 and
;; n-2, each one taking 2^n steps.

;; Excercise 3.58

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den)
           den
           radix)))

;; Expand computes fraction constructed from num and den in regard to
;; the given radix.

;; sicp.rkt> (take 10 (expand 1 7 10))
;; (1 4 2 8 5 7 1 4 2 8)
;; sicp.rkt> (take 10 (expand 3 8 10))
;; (3 7 5 0 0 0 0 0 0 0)


;; sicp.rkt> (/ 1.0 7)
;; 0.14285714285714285
;; sicp.rkt> (/ 3.0 8)
;; 0.375

;; Excercise 3.59

(define (integrate-series s)
  (stream-map * (stream-map / ones integers) s))

(define (iterate x)
  (cons-stream
   x
   (iterate x)))

(define exp-series
  (cons-stream
   1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1
               (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

;; Excercise 3.60
(define (reduce-stream initial op s)
  (let ((r (op initial (stream-car s))))
    (cons-stream
     r
     (reduce-stream r op (stream-cdr s)))))

;; http://community.schemewiki.org/?sicp-ex-3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

;; (take 10 (add-streams (mul-series-lucky cosine-series cosine-series)
;;                       (mul-series-lucky sine-series sine-series)))


;; 1 2 3 4 5
;; 2 2 2 2 2

;; 2 4 6 8 10 -> 2 6 12 20 30

;; 1 2 3 4 5  -> 1 3 6 10 15
;; 2 2 2 2 2  -> 2 4 6 8  10

;;               2 12 36 80 150

;; 1 2 3 4 5
;; 2 2 2 2 2

;; 2 6 12 20
;; -----------
;;   2 2 2 2

;;   2 3 4 5
;;   2 2 2 2 2

;;   4 10 18
;; -------------
;;     4 4 4 4 4

;;     3 4 5
;;     2 2 2

;;     6 14
;;     ----------
;;       6 6 6 6

;;       4 5
;;       2 2

;;       8

;; Excercise 3.61

(define (invert-unit-series s)
  (cons-stream
   1
   (scale-stream
    (mul-series  (stream-cdr s) (invert-unit-series s))
    - 1)))

;; Excercise 3.62

(define (div-series nom denom)
  (if (= (stream-car denom) 0)
      (error "DIVISION BY ZERO")
      (mul-series nom (invert-unit-series denom))))

(define tangent-series
  (div-series sine-series cosine-series))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average a b)
  (/ (+ a b) 2.0))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

;; (take 10 (sqrt-stream 2))

(define (pi-summands n)
  (cons-stream
   (/ 1.0 n)
   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream
   (partial-sums (pi-summands 1)) 4))

(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

;; (take 10 (euler-transform pi-stream))

(define (make-tableau transform s)
  (cons-stream
   s
   (make-tableau
    transform
    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;; (take 10 (accelerated-sequence euler-transform pi-stream))

;; Excercise 3.61

;; Luis version can not be memoized properly (sqrt-stream would call
;; itself without using memo-proc, contrary, when guesses are returned
;; then recursive call to sqrt-stream results in invoking memoized
;; result of guesses is returned). If memoization wasn't used in delay
;; there would be no difference.

;; Excercise 3.64

(define (stream-limit s tolerance)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (compare s1 s2)
    (let ((s1-car (stream-car s1))
          (s2-car (stream-car s2)))
      (if (good-enough? s1-car s2-car)
          s2-car
          (compare (stream-cdr s1) (stream-cdr s2)))))
  (compare s (stream-cdr s)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; Excercise 3.65

(define (ln2-summands n)
  (cons-stream
   (/ 1 n)
   (stream-map - (ln2-summands (+ 1 n)))))

(define ln2-series
  (scale-stream (partial-sums (ln2-summands 1)) 1.0))

(define (display-stream-limit s limit)
  (cond ((= limit 0) 'done)
        (else
         (display (stream-car s))
         (newline)
         (display-stream-limit (stream-cdr s) (- limit 1)))))

(define (stream-limit-display s tolerance)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (compare s1 s2 counter)
    (let ((s1-car (stream-car s1))
          (s2-car (stream-car s2)))
      (if (good-enough? s1-car s2-car)
          (begin
            (display counter)
            (newline)
            s2-car)
          (compare (stream-cdr s1) (stream-cdr s2) (+ counter 1)))))
  (compare s (stream-cdr s) 0))

;; sicp.rkt> (stream-limit-display ln2-series 0.001)
;; 998
;; 0.6926474305598204
;; sicp.rkt> (stream-limit-display ln2-series 0.0001) => does not end in reasonable time

;; sicp.rkt> (stream-limit-display (euler-transform ln2-series) 0.0001)
;; 11
;; 0.6931879423258733

;; sicp.rkt> (stream-limit-display (accelerated-sequence euler-transform ln2-series) 0.0001)
;; 3
;; 0.6931471960735491

;; Infinite streams of pairs

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; Excercise 3.66

;; S(1,1) S(1, 2) S(1,3) S(1,4) S(1,5) S(1,6) S(1,7) S(1,8) S(1,9) S(1,10) -> 4 ahead of next one
;;        S(2, 2) S(2,3) S(2,4) S(2,5) S(2,6) -> 2 ahead of next one
;;                S(3,3) S(3,4) -> 0 ahead of next one
;;                       S(4,4)
;;                         |
;;                         V
;;  3 elems                n     10
;;  2 elems                      5
;;  1 elems                      2
;;  0 elems                      1

;; In general for (n, n) there will be n levels, starting from level
;; with single elem (n, n) at the bottem and each next having twice as
;; much elements as previous. So there would be around 2^n elems.
;; PS. There would be exactly 2^n - 1 elements in such case.

(define (count-to l s)
  (define (same? l1 l2)
    (and (= (car l1) (car l2)) (= (cadr l1) (cadr l2))))
  (define (loop l s c)
    (let ((head (stream-car s)))
      (if (same? head l)
          c
          (loop l (stream-cdr s) (+ 1 c)))))
  (loop l s 1))

;; (count-to '(3 3) (pairs integers integers))
;; (count-to '(4 4) (pairs integers integers))
;; (count-to '(5 5) (pairs integers integers))

;; (n, n): 2^n - 1
;; (n, n + 1): (2^n - 1) + 2^(n - 1)
;; (n, m): (2^n - 1) + 2^(n - 1) + (m - n - 1) * 2^n, where m > n

;; Excercise 3.67

(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x)
                   (list (stream-car s) x))
                 (stream-cdr t))
     (stream-map (lambda (x)
                   (list x (stream-car t)))
                 (stream-cdr s)))
    (all-pairs (stream-cdr s) (stream-cdr t)))))

;; Excercise 3.68

(define (louis-pairs s t)
  (interleave
   (stream-map
    (lambda (x)
      (list (stream-car s) x))
    t)
   (louis-pairs (stream-cdr s)
          (stream-cdr t))))

;; Luis implementation will recurse infinitely. The issue is that
;; interleave function is ordinary function that is not delayed. It
;; means that evaluating cdr of Lois implementation would not return
;; *promise* but would cause inifinte recusion.

;; Excercise 3.69

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map
     (lambda (x)
       (cons (stream-car s) x))
     (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define triples-integers
 (triples integers integers integers))

(define (pythagorean? a b c)
  (= (square c)
     (+ (square a) (square b))))

(define pythagorean-triples
  (stream-filter
   (lambda (x)
     (apply pythagorean? x))
   triples-integers))

;; Excercise 3.70

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1) s2 weight)))
                 (else
                  (cons-stream
                   s2car
                   (merge-weighted s1 (stream-cdr s2) weight)))
                 )))))


(define (weighted-pairs s t w)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map
     (lambda (x)
       (list (stream-car s) x))
     (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) w)
    w)))

(define weighted-pairs-sum
  (weighted-pairs integers integers (lambda (x) (apply + x))))

  (define (not-divisible-2-3-5? x)
    (cond ((= 0 (remainder x 2)) false)
          ((= 0 (remainder x 3)) false)
          ((= 0 (remainder x 5)) false)
          (else true)))
  (define (weight-2-3-5 x)
    (let ((i (car x))
          (j (cadr x)))
      (+ (* 2 i) (* 3 j) (* 5 i j))))

(define weighted-pairs-2-3-5
  (weighted-pairs
   (stream-filter not-divisible-2-3-5? integers)
   (stream-filter not-divisible-2-3-5? integers)
   weight-2-3-5))

;; (take 10 weighted-pairs-2-3-5)

;; Excercise 3.71

(define (cube x)
  (* x x x))
(define (cube-sum x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (cube i) (cube j))))
(define cube-pairs
  (weighted-pairs integers integers cube-sum))


(define (ramanujan)
  (define (loop x s)
    (cond ((= (cube-sum x) (cube-sum (stream-car s)))
           (cons-stream
            (list x (stream-car s) (cube-sum x))
            (loop (stream-car s) (stream-cdr s))))
          (else
           (loop (stream-car s) (stream-cdr s)))))
  (loop (stream-car cube-pairs) (stream-cdr cube-pairs)))

;; (take 6 (ramanujan))

;; Excercise 3.72

(define (square-sum x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (square i) (square j))))

(define square-sum-pairs
  (weighted-pairs integers integers square-sum))

(define (square-triples)
  (define (loop x y s)
    (cond ((= (square-sum x) (square-sum y) (square-sum (stream-car s)))
           (cons-stream
            (list x y (stream-car s) (square-sum x))
            (loop y (stream-car s) (stream-cdr s))))
          (else (loop y (stream-car s) (stream-cdr s)))))
  (loop (stream-car square-sum-pairs) (stream-car (stream-cdr square-sum-pairs)) (stream-cdr (stream-cdr square-sum-pairs))))

;; (take 5 (square-triples))

;; Excercise 3.73 -- 3.80
;; TODO

;; Excercise 3.81
;; (define (random-numbers-generator requests)
;;   (define (random-numbers seed)
;;     (cons-stream seed
;;                  (random-numbers (rand-update seed))))

;;   (define (generate? request)
;;     (eq? request 'generate))

;;   (define (reset? request)
;;     (and (pair? request) (eq? (car request) 'reset)))

;;   (define (loop requests s)
;;     (cond ((stream-null? requests) the-empty-stream)
;;           ((generate? (stream-car requests))
;;            (cons-stream (stream-car s)
;;                         (loop (stream-cdr requests) (stream-cdr s))))
;;           ((reset? (stream-car requests))
;;            (let ((r (random-numbers (cadr (stream-car requests)))))
;;              (cons-stream (stream-car r)
;;                           (loop (stream-cdr requests) (stream-cdr r)))))))

;;   (loop requests (random-numbers 705894)))

;; (define requests
;;   (cons-stream 'generate
;;   (cons-stream 'generate
;;   (cons-stream 'generate
;;   (cons-stream '(reset 705894)
;;   (cons-stream 'generate
;;   (cons-stream 'generate
;;                 the-empty-stream)))))))

;; (display-stream (random-numbers-generator requests))

;; ;; 705894
;; ;; 1126542223
;; ;; 1579310009
;; ;; 705894
;; ;; 1126542223
;; ;; 1579310009
;; ;; done

;; Excercise 3.82
(define (randoms-ranged low high)
  (cons-stream (random-in-range low high)
               (randoms-ranged low high)))

(define (integral-estimates P x1 x2 y1 y2)
  (define point-in-integral-stream
    (stream-map P (randoms-ranged x1 x2) (randoms-ranged y1 y2)))
  (monte-carlo point-in-integral-stream 0 0))

(define pi-integral-estimates
  (stream-map (lambda (area) (/ area (* 0.5 0.5)))
              (integral-estimates in-unit-circle? 0.0 1.0 0.0 1.0)))

;;; 3.5.5 Modularity of Functional Programs and Modularity of Objects
(define (rand-update x)
  (modulo (+ (* 214013 x) 2531011) (expt 2 32)))

(define random-init (rand-update (expt 2 32)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update
                           random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s)
      (stream-car (stream-cdr s)))
   (map-successive-pairs
    f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))


(define (monte-carlo experiment-stream
                     passed
                     failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream)
      passed
      failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p)))
   (stream-cdr (monte-carlo cesaro-stream 0 0))))
