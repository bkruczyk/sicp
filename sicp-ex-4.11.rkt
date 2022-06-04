;; Excercise 4.11
;;
;; I have no idea, making map a primitive works fine for me. Maybe that is
;; because Racket's lang sicp ?
;;
;; Nevertheless I think that codybartfast provides good explanation on SICP
;; solutions site:
;; http://community.schemewiki.org/?sicp-ex-4.14
;;
;; ```
;; I get the following error with Louis's approach:
;;
;;     application: not a procedure;
;;      expected a procedure that can be applied to arguments
;;       given: #0=(mcons 'procedure (mcons (mcons 'x '()) (mcons (mcons (mcons
;;       '* (mcons 'x (mcons 'x (mcons 'x '())))) '()) (mcons (mcons (mcons
;;       '*frame* (mcons (mcons 'cube #0#) (mcons (mcons 'false #f) (mcons
;;       (mcons 'true #t) (mcons (mcons 'map #<procedure:mm...
;;       arguments...:
;;
;; I believe the problem is that Louis is 'crossing the streams'.  He is
;; passing one of 'our' procedures to a primitive procedure.
;;
;; We have a procedure, cube, that is designed to be applied in the
;; implementation that we are constructing.  But here we are passing cube (i.e.
;; the procedure object referenced by the symbol 'cube) to the underlying map.
;; So it will be the underlying implementation (e.g. Racket, Guile, Chicken,
;; ...) that will apply map and, in turn, attempt to apply our cube procedure.
;; This cannot work if our implementation of procedures, environments, etcetera
;; is different from the ones used by the underlying implementation.
;;
;; (If key parts of the underlying implementation were identical to our
;; implementation I can imagine that it might work, but clearly we should never
;; rely on that.)
;; ```
