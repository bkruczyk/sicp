(define (run-forever)
  (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

;; assuming that (halts? try try) is true then (try try) runs forover which
;; contradicts the premise that we can determine if programs stops or not

;; assuming that (halts? try try) is false then (try try) should stop execution
;; with 'halted which contradicats the premise that (try try) does run forever
