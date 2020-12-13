#lang sicp

(define (for-each f items)
  (if (null? items)
      #f
      (or (and (f (car items)) #f)
          (for-each f (cdr items)))))
