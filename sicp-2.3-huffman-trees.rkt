#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits)
                current-branch)))
          (if (leaf? next-branch)
              (cons
               (symbol-leaf next-branch)
               (decode-1 (cdr bits)
                         tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)
                    (cadr pair))
         (make-leaf-set (cdr pairs))))))

;; (make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

;; excercise 2.67
(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))
(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; (decode sample-message sample-tree)
;; (A D A B B C A)


;; excercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(define (contains? symbol symbol-set)
  (cond ((null? symbol-set) false)
        ((eq? symbol (car symbol-set)) true)
        (else (contains? symbol (cdr symbol-set)))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) nil)
        ((contains? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contains? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "NO SYMBOL: encode-symbol: " symbol))))

;; excercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
        ((= (length leaf-set) 1) (car leaf-set))
        (else
         (successive-merge
          (adjoin-set
           (make-code-tree (car leaf-set) (cadr leaf-set))
           (cddr leaf-set))))))

;; excercise 2.69
(define rock50-tree
  (generate-huffman-tree '((A 2)
                           (BOOM 1)
                           (GET 2)
                           (JOB 2)
                           (NA 16)
                           (SHA 3)
                           (YIP 9)
                           (WAH 1))))
;; (length '(GET A JOB
;;           SHA NA NA NA NA NA NA NA NA

;;           GET A JOB
;;           SHA NA NA NA NA NA NA NA NA

;;           WAH YIP YIP YIP YIP
;;           YIP YIP YIP YIP YIP
;;           SHA BOOM)) => 36

;; (length (encode '(
;;                  GET A JOB
;;                  SHA NA NA NA NA NA NA NA NA

;;                  GET A JOB
;;                  SHA NA NA NA NA NA NA NA NA

;;                  WAH YIP YIP YIP YIP
;;                  YIP YIP YIP YIP YIP
;;                  SHA BOOM
;;                  ) rock50-tree)) => 84 bits

;; How many bits are required for the encoding?
;; => 84
;;
;; What is the smallest number of bits that would be needed to encode this song
;; if we used a fixed-length code for the eight-symbol alphabet?
;; => 8 symbols can be coded on 3 bits, there are 36 word in the song so
;; 3 * 36 = 108 bits total

;; excercise 2.71
;;
;; n = 5

;; a 1
;; b 2
;; c 4
;; d 8
;; e 16

;; a b 3                           (a b).3
;; c 4                              /     \
;; d 8                           (a).1   (b).2
;; e 16

;; a b c 7                      (a b c) . 7
;; d 8                            /       \
;; e 16                     (a b).3      (c).4
;;                         /     \
;;                      (a).1   (b).2

;; a b c d 15
;; e 16                               (a b c d).15
;;                                     /         \
;;                                (a b c).7     (d).8
;;                                 /       \
;;                             (a b).3      (c).4
;;                            /     \
;;                         (a).1   (b).2

;; a b c d e 31
;;                                        (a b c d e).31
;;                                          /          \
;;                                  (a b c d).15        (e).16
;;                                     /         \
;;                                (a b c).7     (d).8
;;                                 /       \
;;                             (a b).3      (c).4
;;                            /     \
;;                         (a).1   (b).2

;; n = 10
;; enough is enough ;)
;;
;; with such frequency distribution most frequent symbol requires one bit, least
;; common symbols requires number of bits equal to number of symbols
;;
;;
;; excercise 2.72
;;
;; assuming tree from prevous excercise and most frequent symbol
;; we need to just travel one bit down, so starting from the top:
;; - contains? in big O notation needs O(n) where n = number of symbols
;; - check of the leaf is O(1)
;;
;; in least frequent symbol case it would be n x O(n) so O(n^2)
;;
;; if encoding tree would be balanced then depth of any leaf would be
;; log2(2^n) so complexity would be then O(nlogn)
