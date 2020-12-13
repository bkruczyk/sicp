#lang sicp
(#%require racket/trace)

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (attach-tag type-tag contents)
  (if (number? contents) contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum)
         (car datum))
        (else (error "Bad tagged datum:
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum)
         (cdr datum))
        (else (error "Bad tagged datum:
              CONTENTS" datum))))

;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           (error
;;             "No method for these types:
;;              APPLY-GENERIC"
;;             (list op type-tags))))))


;;;
(define coercion-table '())
(define (put-coercion type1 type2 item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! coercion-table (put-helper (list type1 type2) coercion-table)))

(define (get-coercion type1 type2)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list type1 type2) coercion-table))

;; apply-generic redefinition for coercion
(define (apply-generic op . args)
  (define (apply-coercion a1 type1 a2 type2)
    (let ((t1->t2
           (get-coercion type1
                         type2))
          (t2->t1
           (get-coercion type2
                         type1)))
      (cond ((equal? type1 type2)
             (error "Coercion for same types" (list op type1 type2)))
            (t1->t2
             (apply-generic
              op (t1->t2 a1) a2))
            (t2->t1
             (apply-generic
              op a1 (t2->t1 a2)))
            (else
             (error "No method for these types" (list op type1 type2))))))
  (define (apply-coercion-raise a1 type1 a2 type2)
    (let ((lesser (cadr (min-type type1 type2))))
      (cond ((eq? lesser type1)
             (apply apply-generic (list op (raise a1) a2)))
            ((eq? lesser type2)
             (apply apply-generic (list op (raise a2) a1))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (= (length args) 2)
                ;; (apply-coercion a1 type1 a2 type2)
                (apply-coercion-raise a1 type1 a2 type2)
                (apply apply-generic (append (list op (apply-generic op a1 a2)) (cddr args)))))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))
(define (square n) (* n n))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (sine x)
  (apply-generic 'sine x))
(define (cosine x)
  (apply-generic 'cosine x))

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'exp
       '(scheme-number scheme-number)
       (lambda (x y)
         (tag (expt x y))))
  (put 'mul-varargs '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'raise '(scheme-number) (lambda (x) (make-rational x 1)))
  (put 'sine '(scheme-number) sin)
  (put 'cosine '(scheme-number) cos)
  'done)

(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (eq-rational x y)
   (and (= (numer x) (numer y))
        (= (denom x) (denom y))))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (put 'numer-part '(rational) numer)
  (put 'denom-part '(rational) denom)
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ '(rational rational) eq-rational)
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
  (put 'mul-varargs '(rational rational) mul-rat)
  (put 'raise '(rational) (lambda (x) (make-complex-from-real-imag (/ (numer x) (denom x)) 0)))
  (put 'project '(rational) (lambda (x) (numer x)))
  (put 'sine '(rational) (lambda (x) (sin (/ (numer x) (denom x)))))
  (put 'cosine '(rational) (lambda (x) (cos (/ (numer x) (denom x)))))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  (define (eq-complex x y)
    (and (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'equ '(complex complex) eq-complex)
  (put '=zero? '(complex) (lambda (x) (and (= 0 (real-part x)) (= 0 (imag-part x)))))
  (put 'project '(complex)
       (lambda (x) (make-rational (real-part x) 1)))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-complex-package)
(install-rectangular-package)
(install-polar-package)

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))
(define (numer-part x)
  (apply-generic 'numer-part x))
(define (denom-part x)
  (apply-generic 'denom-part x))


;; excercise 2.77

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

;; (trace magnitude)
;; (trace apply-generic)

;; (magnitude (make-complex-from-real-imag 3 4))

;; magnitude is first matched with complex tag, stripped from it and magnitude
;; that works for rectangular is called, apply-generic is called two times here

;; excercise 2.78
;; done upper

;; excercise 2.79

(define (equ? x y)
  (apply-generic 'equ x y))

;; modification to packages done above

;; excercise 2.80

(define (=zero? x)
  (apply-generic '=zero? x))

;; excercise 2.81

;; (define (scheme-number->scheme-number n) n)
;; (define (complex->complex z) z)

;; (put-coercion 'scheme-number 'scheme-number
;;               scheme-number->scheme-number)

;; (put-coercion 'complex 'complex
;;               complex->complex)

;; (define (exp x y)
;;   (apply-generic 'exp x y))

;; (trace exp)
;; (trace get)
;; (trace get-coercion)

;; (exp (make-complex-from-real-imag 2 0) (make-complex-from-real-imag 3 0))
;; (exp 2 3)

;; > With Louis’s coercion procedures installed, what happens if apply-generic
;; is called with two arguments of type scheme-number or two arguments of type
;; complex for an operation that is not found in the table for those types?

;; > What happens if we call exp with two complex numbers as arguments?
;;
;;No method for these types (exp (complex complex)) error is thrown without Luis
;;changes applied. With Luis changes applied program falls into endless loop
;;while trying to coerce (complex complex) type tags

;; > Is Louis correct that something had to be done about coercion with
;; arguments of the same type, or does apply-generic work correctly as is?
;;
;; apply-generic work correctly as it is
;;
;; > Modify apply-generic so that it doesn’t try coercion if the two arguments
;; have the same type.
;;
;; apply-generic modified above
;;

;; excercise 2.81
;;
;; > One strategy is to attempt to coerce all the arguments to the type of the
;; first argument, then to the type of the second argument, and so on.
;;
;; I don't get the point of coercing all arguments to the type of first argument
;; "and so on". Instead I'm coercing second argument to first, third to product
;; of first and second etc.
;;
;; > Give an example of a situation where this strategy (and likewise the
;; two-argument version given above) is not sufficiently general.
;;
;; Probably sometimes it is more sensible to coerce to some intermediete type
;; instead of simply using coercion table.

;; excercise 2.82
;; changes to apply-generic done above
(define (mul-varargs . args)
  (apply apply-generic (cons 'mul-varargs args)))
(put-coercion 'scheme-number 'rational (lambda (n) (make-rational n 1)))

;; excercise 2.83
;; implemented for scheme-number
(define (raise arg)
  (apply-generic 'raise arg))

;; excercise 2.84
(define num-types '(scheme-number rational real complex))

(define (min-type a b)
  (define (recur a b types tuple)
    (cond ((null? types)
           tuple)
          ((eq? a (car types))
           (recur a b (cdr types) (cons a tuple)))
          ((eq? b (car types))
           (recur a b (cdr types) (cons b tuple)))
          (else (recur a b (cdr types) tuple))))
  (recur a b num-types (list)))

;; apply-coercion-raise implemented in apply-generic method above

;; excercise 2.85

(define (project n)
  (apply-generic 'project n))

(define (can-drop? x)
  (and (not (eq? (type-tag x) (car num-types)))
       (equ? (raise (project x)) x)))

(define (drop x)
  (if (can-drop? x)
      (drop (project x))
      x))

;; drop added in apply-generic above
;; aaand it causing infinite loop!
;; eg. (can-drop? (make-rational 2 1))
;; calls project using apply-generic
;; which tries to drop answer...
;; using can-drop? function!
;;
;; to overcome this one would need to
;; check if proc in apply-generic in fact
;; raise function

;; excercise 2.86
;; sine and cosine implmented using apply-generic above

;;
(define (install-polynomial-package)
  ;; internal procedures
  ;; representations of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (display term) (car term))
  (define (coeff term) (cadr term))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1
                     (add-terms (rest-terms L1)
                                L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2
                     (add-terms
                      L1
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term
                      (order t1)
                      (add (coeff t1)
                           (coeff t2)))
                     (add-terms
                      (rest-terms L1)
                      (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms
         (mul-term-by-all-terms
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms
            t1
            (rest-terms L))))))
  (define (zero-poly? p)
    (=zero? (accumulate + 0 (map coeff (term-list p)))))
  ;; interface to rest of system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) zero-poly?)
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)

;; I OMIT SECTION 2.5.4 EXAMPLE: SYMBOLIC ALGEBRA
;; IT HAS BEEN LONG CHAPTER ALREADY
