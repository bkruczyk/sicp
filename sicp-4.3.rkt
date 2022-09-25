#lang sicp

(define (prime? n)
  (error "PRIME: not defined"))

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

(define (require p)
  (if (not p)
      (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items)
       (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; Excercise 4.35

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j))
                    (* k k)))
        (list i j k)))))

(define (an-integer-between low high)
  (require (< low high))
  (amb low (an-integer-between (+ low 1) high)))

;; Excercise 4.36

;; Simply replacing `an-integer-between` with `an-integer-starting-from` will
;; not be sufficent because `an-integer-starting-from` is unbounded and calling
;; `try-again` will affect only the last `an-integer-starting-from`. It will
;; result in triplets (1,1,1) (1,1,2) (1,1,3) (1,1,4) etc.

(define (a-pythagorean-triples)
  (let ((triple (a-triples 1 1 1)))
    (let ((i (car triple))
          (j (cadr triple))
          (k (caadr triple)))
      (require (= (+ (* i i) (* j j))
                  (* k k)))
      (list i j k))))

;; TODO Come back to this excercise after implementing amb evaluator!
;; TODO Draw diagram depicting how amb evaluation would look like. Keep in mind that:
;; """
;; When the evaluator encounters an application of amb, it initially selects the
;; first alternative. This selection may itself lead to a further choice. The
;; evaluator will always initially choose the first alternative at each choice
;; point. If a choice results in a failure, then the evaluator automagically.
;; backtracks to the most recent choice point and tries the next alternative. If
;; it runs out of alternatives at any choice point, the evaluator will back up
;; to the previous choice point and resume from there. This process leads to a
;; search strategy known as depth-first search or chronological backtracking.
;; """

;; (1, 1) (1, 2) (1, 3)
;;        (2, 2) (2, 3)

;; start with (1, 1)
;; now let's try to inc first item and carry-over second item (2, 1)
;; we check that 2 <= 1 is false -> fail and back track
;; now let's do (1, 2), 1 <= 2 is true so we proceed again with inc first item and carry over second item (2, 2)
;; we check thet 2 <= 2 is true so we proceed with (3, 2)
;; 3 <= 2 fail so we back track
;; etc etc

(define (a-pairs i j)
  (require (<= i j))
  (amb (list i j)
       (a-pairs (+ i 1) j)
       (a-pairs (i (+ j 1)))))

(define (a-triples i j k)
  (require (and (<= i j)
                (<= j k)))
  (amb (list i j k)
       (a-triples (+ i 1) j k)
       (a-triples i (+ j 1) k)
       (a-triples i j (+ k 1))))

;; Excercise 4.37
;;
;; Ben is right since in algorithm from version 4.35 we must consider i * j * k
;; cases, but in Ben's version of algorithm we only consider i * j cases (plus 2
;; checks for (>= hsq ksq) and (integer? k).

;; Excercise 4.38

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling-modified)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher
                      miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require
     (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; Total number of solutions without any restrictions is 5! = 125
;;
;; All cases where smith and fletcher are living on the adjacent floor
;;
;; 1 -- smith
;; 2 -- fletcher
;;
;; 2 -- smith
;; 3 -- fletcher
;;
;; 3 -- smith
;; 4 -- fletcher
;;
;; 4 -- smith
;; 5 -- fletcher
;;
;; x2 since we can switch smith and fletcher = 8
;;
;; So it only lower total number of cases by 8.

;; Excercise 4.39
;;
;; Order of restrictions does not matter in this case since there is no further
;; computation depending on the restrictions. Time complexity for this procedure
;; comes from the number of possibilities generated in the first step.

;; Excercise 4.40
;;
;; Total number of generated cases is 5 * 5 * 5 * 5 * 5 = 3125.
;; Total number of distinct cases is 5! = 125

(define (a-multiple-dwelling)
  (let ((fletcher (amb 2 3 4)))
    (let ((cooper (amb 2 3 4 5)))
      (require (distinct? (list cooper fletcher)))
      (require (not (= 1 (abs (- fletcher cooper)))))
      (let ((baker (amb 1 2 3 4)))
        (require (distinct? (list baker cooper fletcher)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (> miller cooper))
          (require (distinct? (list miller baker cooper fletcher)))
          (let ((smith (amb 1 2 3 4 5)))
            (require (distinct? (list smith miller baker cooper fletcher)))
            (require (not (= 1 (abs (- fletcher smith)))))

            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

;; Excercise 4.41
;;
;; "Naive" way, could be improved by adding checks between lambdas eg. (if (= fletcher cooper) (list)).

(define (multiple-dwelling-ordinary-scheme)
  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))
  (define (flatmap proc seq)
    (accumulate append nil (map proc seq)))
  (define (filter pred seq)
    (if (null? seq)
        seq
        (if (pred (car seq))
            (cons (car seq) (filter pred (cdr seq)))
            (filter pred (cdr seq)))))
  (filter (lambda (seq) (not (null? seq)))
          (flatmap (lambda (fletcher)
                     (flatmap (lambda (cooper)
                                (flatmap (lambda (baker)
                                           (flatmap (lambda (miller)
                                                      (map (lambda (smith)
                                                             (let ((solution (list 'fletcher fletcher
                                                                                   'cooper cooper
                                                                                   'baker baker
                                                                                   'miller miller
                                                                                   'smith smith)))
                                                               (if (and (distinct? solution)
                                                                        (> miller cooper)
                                                                        (not (= 1 (abs (- fletcher cooper))))
                                                                        (not (= 1 (abs (- fletcher smith)))))
                                                                   solution
                                                                   (list))))
                                                           (list 1 2 3 4 5)))
                                                    (list 1 2 3 4 5)))
                                         (list 1 2 3 4)))
                              (list 2 3 4 5)))
                   (list 2 3 4))))
