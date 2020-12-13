#lang sicp

(#%require racket/trace)

(define (memq item x)
  (cond ((null? x) false)

        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; excercise 2.54

(define (equal? xs ys)
  (cond ((and (pair? xs) (pair? ys))
         (and (eq? (car xs) (car ys))
              (equal? (cdr xs) (cdr ys))))
        ((and (not (pair? xs)) (not (pair? ys)))
         (eq? xs ys))
        (else #f)))

;; excercise 2.55
;; 'abracadabra is a shorthand for (quote abreacadabra)
;; ''abreacadabra expands to (quote (quote abracadabra))
;; it looks that this is a pair in the sense of car and cdr
;; (car (quote (quote abracadabra))) => quote
;; (cdr (quote (quote abracadabra))) => (abracadabra)
;; so looks like quoting itself makes a list

;; after I've looked up solution: what is happening is that in (quote (quote
;; abracadabra)) first quote quotes expression (quote abracadabra) which is a
;; list, then doing car on it it return first element of this list which is
;; quote

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation
            (base exp)
            (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else (error "unknown expression
                      type: deriv" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;; (define (make-sum a1 a2) (list '+ a1 a2))
;; (define (make-product m1 m2) (list '* m1 m2))

;; (define (sum? x)
;;   (and (pair? x) (eq? (car x) '+)))

;; (define (addend s) (make-sum (cadr s)))
;; (define (augend s) (apply make-sum (cddr s)))
;; (define (product? x)
;;   (and (pair? x) (eq? (car x) '*)))

;; (define (multiplier p) (make-product (cadr p)))
;; (define (multiplicand p) (apply make-product (cddr p)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0) a2)
;;         ((=number? a2 0) a1)
;;         ((and (number? a1) (number? a2))
;;          (+ a1 a2))
;;         (else (list '+ a1 a2))))

;; (define (make-sum-a as)
;;   (if (null? as) as
;;       (let ((first (car as))
;;             (rest (make-sum-a (cdr as))))
;;         (cond ((null? rest)
;;                (list first))
;;               ((=number? first 0)
;;                rest)
;;               ((=number? (car rest) 0)
;;                (list first))
;;               ((and (number? (car rest)) (number? first))
;;                (cons (+ first (car rest)) (cdr rest)))
;;               ((number? first)
;;                (cons (car as) rest))
;;               (else (append rest (list first)))))))

;; (define (make-sum . as)
;;   (let ((sum (make-sum-a as)))
;;     (cond ((null? sum) 0)
;;           ((null? (cdr sum)) (car sum))
;;           (else (cons '+ sum)))))

;; (trace make-sum-a)
;; (trace add-into)

;; (define (make-product m1 m2)
;;   (cond ((or (=number? m1 0)
;;              (=number? m2 0))
;;          0)
;;         ((=number? m1 1) m2)
;;         ((=number? m2 1) m1)
;;         ((and (number? m1) (number? m2))
;;          (* m1 m2))
;;         (else (list '* m1 m2))))

;; (define (make-product-a as)
;;   (if (null? as) as
;;       (let ((rest (make-product-a (cdr as))))
;;         (cond ((null? rest)
;;                (list (car as)))
;;               ((=number? (car as) 0)
;;                0)
;;               ((=number? (car as) 1)
;;                rest)
;;               ((=number? (car rest) 0)
;;                (list 0))
;;               ((=number? (car rest) 1)
;;                (cons (car as) (cdr rest)))
;;               ((and (number? (car rest)) (number? (car as)))
;;                (cons (* (car as) (car rest)) (cdr rest)))
;;               ((number? (car as))
;;                (cons (car as) rest))
;;               (else (append rest (list (car as))))))))

;; (define (make-product . as)
;;   (let ((sum (make-product-a as)))
;;     (cond ((null? sum) 0)
;;           ((null? (cdr sum)) (car sum))
;;           (else (cons '* sum)))))

;; excercise 2.56
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base p)
  (cadr p))
(define (exponent p)
  (caddr p))
(define (** base exponent)
  (cond ((= exponent 0) 1)
        ((= base 1) 1)
        (else (* base (** base (- exponent 1))))))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        ((and (number? base) (number? exponent)) (** base exponent))
        (else (list '** base exponent))))

;; excercise 2.57
;; I don't want to waste any more time on this one, looks good enough to me

;; excercise 2.58
;; 1)

;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0) a2)
;;         ((=number? a2 0) a1)
;;         ((and (number? a1) (number? a2))
;;          (+ a1 a2))
;;         (else (list a1 '+ a2))))
;; (define (addend exp)
;;   (car exp))
;; (define (augend exp)
;;   (caddr exp))

;; (define (make-product m1 m2)
;;   (cond ((or (=number? m1 0)
;;              (=number? m2 0))
;;          0)
;;         ((=number? m1 1) m2)
;;         ((=number? m2 1) m1)
;;         ((and (number? m1) (number? m2))
;;          (* m1 m2))
;;         (else (list m1 '* m2))))

;; (define (multiplier p) (car p))
;; (define (multiplicand p) (caddr p))

;; (define (sum? x)
;;   (and (pair? x) (eq? (cadr x) '+)))
;; (define (product? x)
;;   (and (pair? x) (eq? (cadr x) '*)))

;; 2)
;; this solution is wrong since those functions should take varargs

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))
(define (addend exp)
  (car exp))
(define (augend exp)
  (let ((rest (cddr exp)))
    (if (= 1 (length rest))
        (car rest)
        rest)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (multiplier p) (car p))
(define (multiplicand p)
  (let ((rest (cddr p)))
    (if (= 1 (length rest))
        (car rest)
        rest)))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

;;
;;
;;

;; (define (element-of-set? x set)
;;   (cond ((null? set) false)
;;         ((equal? x (car set)) true)
;;         (else (element-of-set? x (cdr set)))))

;; (define (adjoin-set x set)
;;   (if (element-of-set? x set)
;;       set
;;       (cons x set)))

;; (define (intersection-set set1 set2)
;;   (cond ((or (null? set1) (null? set2))
;;          '())
;;         ((element-of-set? (car set1) set2)
;;          (cons (car set1)
;;                (intersection-set (cdr set1)
;;                                  set2)))
;;         (else (intersection-set (cdr set1)
;;                                 set2))))

;; excercise 2.59

;; (define (union-set set1 set2)
;;   (cond ((and (null? set1) (null? set2)) '())
;;         ((null? set1) set2)
;;         ((null? set2) set1)
;;         (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

;; ;; excercise 2.60

;; ;; procedure element-of-set? stays the same

;; ;; adjoin-set is simply cons
;; (define (260/adjoin-set x set) (cons x set))

;; ;; union set is just append
;; (define (260/union-set set1 set2) (append set1 set2))

;; ;; this is fun
;; (define (260/interesction-set set1 set2)
;;   (260/union-set (intersection-set set1 set2)
;;                  (intersection-set set2 set1)))

;; adjoin-set and union-set are fast sice they are O(1)
;; no idea for what reason I should use set with duplicates

;; excercise 2.61

;; the implementation of adjoin-set can be exactly the same given
;; definition of element-of-set? for ordered list representation
;;
;; => NOT TRUE since adjoined element needs to be added at correct position
;; see excercise 2.65 for solution

;; excercise 2.62

;; (define (262/union-set set1 set2)
;;   (cond ((and (null? set1) (null? set2)) '())
;;         ((null? set1) set2)
;;         ((null? set2) set1)
;;         (else
;;          (let ((x1 (car set1))
;;                (x2 (car set2)))
;;            (cond ((= x1 x2)
;;                   (cons x1 (262/union-set (cdr set1) (cdr set2))))
;;                  ((< x1 x2)
;;                   (cons x1 (262/union-set (cdr set1) set2)))
;;                  (else
;;                   (cons x2 (262/union-set set1 (cdr set2)))))))))

;;
;;
;;

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; (define (element-of-set? x set)
;;   (cond ((null? set) false)
;;         ((= x (entry set)) true)
;;         ((< x (entry set))
;;          (element-of-set? x (left-branch set)))
;;         ((> x (entry set))
;;          (element-of-set? x (right-branch set)))))
;; (define (adjoin-set x set)
;;   (cond ((null? set) (make-tree x '() '()))
;;         ((= x (entry set)) set)
;;         ((< x (entry set))
;;          (make-tree
;;           (entry set)
;;           (adjoin-set x (left-branch set))
;;           (right-branch set)))
;;         ((> x (entry set))
;;          (make-tree
;;           (entry set)
;;           (left-branch set)
;;           (adjoin-set x (right-branch set))))))

;; excercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
       (tree->list-1 (left-branch tree))
       (cons (entry tree)
             (tree->list-1
              (right-branch tree))))))

;; on each tree level we need to append result of left-branch and cons of entry
;; and result of right branch
;;
;; let's assume append and cons are O(1)
;;
;; we have (assuming balanced tree) log n levels and on each of them we need to
;; do as many append-cons as we have leaves so one of first level, two on
;; second, four on third etc, which is O(n) operations
;;
;; this gives us O(n log n)
;;
;; another take in this could be so that we have tree like following with 8 elements
;;              n
;;            /   \
;;          n/2   n/2
;;         /  \  /   \
;;       n/4 n/4 n/4  n/4
;;
;; one could say that it constitutes tree with 2n - 1 "nodes" at which you need
;; to perform O(1) operation (append-cons) which in total is just O(n)

(define (copy-to-list tree result-list)
  (if (null? tree)
      result-list
      (copy-to-list
       (left-branch tree)
       (cons (entry tree)
             (copy-to-list
              (right-branch tree)
              result-list)))))

(define (tree->list-2 tree)
  (copy-to-list tree '()))

;; copy-to-list travels tree depth-first and is adding each leave to resulting
;; list one-by-one using cons which is simply O(n) difference between two
;; procedures is in append usage in first procedure to merge partial result
;; lists whereas second procedure adds elements one by one

(define t1 (make-tree 7 '(3 (1 () ()) (5 () ())) '(9 () (11 () ()))))
(define t2 (make-tree 3 '(1 () ()) '(7 (5 () ()) (9 () (11 () ())))))
(define t3 (make-tree 5 '(3 (1 () ()) ()) '(9 (7 () ()) (11 () ()))))

;; lists produced by two procedures are the same

;; excercise 2.64
(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      ;; if requested number of elements to processed is zero
      ;; then just return what we have in elements
      (cons '() elts)
      ;; size of elements on the left from this-entry
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          ;; tree built from the elements on the left from entry
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              ;; tree built from the elements to the right of entry
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;; excercise 2.65
;;
;; assuming that procedures from 2.63 and 2.64 are O(n) we can
;; just convert to and from (ordered) list back and forth

(define (262/element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (262/element-of-set? x (cdr set)))))

(define (262/adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (262/adjoin-set x (cdr set))))))

(define (262/union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (262/union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (262/union-set (cdr set1) set2)))
                 (else
                  (cons x2 (262/union-set set1 (cdr set2)))))))))

(define (262/intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (262/intersection-set
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (262/intersection-set
                          (cdr set1)
                          set2))
              ((< x2 x1) (262/intersection-set
                          set1
                          (cdr set2)))))))


(define (265/union-set s1 s2)
  (list->tree
   (262/union-set (tree->list-1 s1)
                  (tree->list-1 s2))))

(define (265/intersection-set s1 s2)
  (list->tree
   (262/intersection-set (tree->list-1 s1)
                         (tree->list-1 s2))))

;;;

;; excercise 2.66

(define (make-record key value)
  (cons key value))

(define (record-key record)
  (car record))
(define (record-value record)
  (cdr record))


(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key
                 (key (car set-of-records)))
         (car set-of-records))
        (else
         (lookup given-key
                 (cdr set-of-records)))))
