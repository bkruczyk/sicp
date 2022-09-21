#lang sicp


(define apply-in-underlying-scheme apply)

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (eval exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (text-of-quotation exp))
        ((assignment? exp)
         (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp)
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence
          (begin-actions exp)
          env))
        ((cond? exp)
         (eval (cond->if exp) env))
        ((application? exp)
         (meta-apply (actual-value (operator exp) env)
                     (operands exp)
                     env))
        (else
         (error "Unknown expression
                 type: EVAL" exp))))

;; renamed from apply because of racket quirk:
;; https://stackoverflow.com/questions/7171705/how-to-run-the-metacircular-evaluator-in-drracket
;; (define (meta-apply procedure arguments)
;;   (cond ((primitive-procedure? procedure)
;;          (apply-primitive-procedure
;;           procedure
;;           arguments))
;;         ((compound-procedure? procedure)
;;          (eval-sequence
;;           (procedure-body procedure)
;;           (extend-environment
;;            (procedure-parameters
;;             procedure)
;;            arguments
;;            (procedure-environment
;;             procedure))))
;;         (else
;;          (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values
             (rest-operands exps)
             env))))

;; (define (eval-if exp env)
;;   (if (true? (eval (if-predicate exp) env))
;;       (eval (if-consequent exp) env)
;;       (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps)
                        env))))

(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
       (cdadr exp)
       (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate
                 consequent
                 alternative)
  (list 'if
        predicate
        consequent
        alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false    ;no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (if (cond-recipient-clause? first)
                         (list (cond-recipient first) (cond-predicate first)) ; cond-predicate will be executed twice, will not work if predicate is stateful
                         (sequence->exp
                          (cond-actions first)))
                     (expand-clauses
                      rest))))))
(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

(define (and->if exp)
  (expand-and (cdr exp)))
(define (expand-and terms)
  (if (null? terms)
      #t
      (let ((first (car terms))
            (rest (cdr terms)))
        (if (null? rest)
            (make-if first first #f)
            (make-if first (expand-and rest) #f)))))

(define (or->if exp)
  (expand-or (cdr exp)))
(define (expand-or terms)
  (if (null? terms)
      #f
      (let ((first (car terms))
            (rest (cdr terms)))
        (make-if first
                 first
                 (expand-or rest)))))

(define (cond-recipient-clause? clause)
  (eq? '=> (cadr clause)))
(define (cond-recipient clause)
  (caddr clause))

(define (let? exp)
  (tagged-list? exp 'let))

;; TODO Check if bindings are pairs
(define (let-bindings exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (named-let? exp)
      (cadddr exp)
      (cddr exp)))

(define (bindings->params bindings)
  (if (null? bindings)
      bindings
      (cons
       (caar bindings)
       (bindings->params (cdr bindings)))))

(define (bindings->args bindings)
  (if (null? bindings)
      bindings
      (cons
       (cadar bindings)
       (bindings->args (cdr bindings)))))

(define (named-let? exp)
  (not (pair? (cadr exp))))

(define (let-var exp)
  (cadr exp))

(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination exp)
      (cons (make-lambda
              (bindings->params (let-bindings exp))
              (let-body exp))
            (bindings->args (let-bindings exp)))))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (make-let bindings body)
  (list 'let bindings body))

(define (let*->nested-lets exp)
  (expand-lets (let-bindings exp)
               (car (let-body exp))))

(define (expand-lets bindings body)
  (if (null? bindings)
      body
      (make-let
       (list (car bindings))
       (expand-lets (cdr bindings) body))))

(define (named-let->combination exp)
  (cons
   (list
    (make-lambda '(f) '((f f)))
    (make-lambda (list (let-var exp))
                 (list (make-lambda (bindings->params (let-bindings exp))
                                    (list
                                     (list (make-lambda (list (let-var exp))
                                                        (list (let-body exp)))
                                           (list (let-var exp) (let-var exp))))))))
   (bindings->args (let-bindings exp))))

;;; 4.1.3 Evaluator Data Structures

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (internal-definitions procedure-body)
  (if (not (pair? procedure-body)) nil
      (let ((exp (car procedure-body)))
        (cond ((definition? exp)
               (cons exp (internal-definitions (cdr procedure-body))))
              (else nil)))))
(define (without-internal-definitions procedure-body)
  (if (not (pair? procedure-body)) procedure-body
      (let ((exp (car procedure-body)))
        (cond ((definition? exp)
               (without-internal-definitions (cdr procedure-body)))
              (else exp)))))

(define (into-unassigned-list internal-definitions)
  (map (lambda (def) (list (cadr def) ''*unassigned*)) internal-definitions))
(define (into-set-expressions internal-definitions)
  (map (lambda (def)
         (list 'set! (cadr def) (caddr def)))
       internal-definitions))

(define (scan-out-defines procedure-body)
  (let ((definitions (internal-definitions procedure-body))
        (body (without-internal-definitions procedure-body)))
    (if (null? definitions) procedure-body
        (list
         (append
          (list 'let
                (into-unassigned-list definitions))
          (append (into-set-expressions definitions)
                  (list body)))))))


(define (make-procedure parameters body env)
  (list 'procedure parameters
        (scan-out-defines body)
        env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vars))
          (error "Too many arguments supplied"
                 vars
                 vals)
          (error "Too few arguments supplied"
                 vars
                 vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame!
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'newline newline)
        (list '+ +)
        (list '- -)
        (list '= =)
        (list 'display display)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;; (define input-prompt ";;; M-Eval input:")
;; (define output-prompt ";;; M-Eval value:")

(define (prompt-for-input string)
  (newline) (newline)
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))

(define the-global-environment
  (setup-environment))

;; (define (driver-loop)
;;   (prompt-for-input input-prompt)
;;   (let ((input (read)))
;;     (let ((output
;;            (eval input
;;                  the-global-environment)))
;;       (announce-output output-prompt)
;;       (user-print output)))
;;   (driver-loop))

;;;

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (meta-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values
           arguments
           env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args
            arguments
            env)   ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure
                      type: APPLY"
                     procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value
             (first-operand exps)
             env)
            (list-of-arg-values
             (rest-operands exps)
             env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it
             (first-operand exps)
             env)
            (list-of-delayed-args
             (rest-operands exps)
             env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp)
                           env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define input-prompt  ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value
                   input
                   the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; (define (force-it obj)
;;   (if (thunk? obj)
;;       (actual-value (thunk-exp obj)
;;                     (thunk-env obj))
;;       obj))

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result
                (actual-value
                 (thunk-exp obj)
                 (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           ;; replace exp with its value:
           (set-car! (cdr obj) result)
           ;; forget unneeded env:
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;; Excercise 4.27

(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define w (id (id 10)))

;; when w is defined count does not change
;; as definition does not need to force thunk
;; count    -> 0

;; value of w is it's argument, so 10
;; while evaluating w, first (id 10) was evaluated
;; which incremented count from 0 to 1,
;; after that when (id (id 1)) was evaluated then count was incremented
;; from 1 to 2
;; w        -> 10

;; count    -> 2

;; Excercise 4.28

;; Eg. Operator can be higher order function with it arg being a function with thunk?
;; (partial (filter (lambda (x) (= x 2))))

;; Excercise 4.29
;;
;; Eg. Recursive fibonnaci procedure.
;;
;; Without memoization count is 2, with it is 1.

;; Excercise 4.30
;; My answers might be wrong, check:
;; http://community.schemewiki.org/?sicp-ex-4.30

;; 1) For-each works correct because our evaluator forces operators.

;; (define (for-each proc items)
;;   (if (null? items)
;;       'done
;;       (begin (proc (car items))
;;              (for-each proc
;;                        (cdr items)))))

;; 2) Original evaluator
;; (p1 1) -> '(1 2)
;; (p2 1) -> 1

;; Cy's changes
;; (p1 1) -> '(1 2)
;; (p2 1) -> '(1 2)

;; 3) Because set! and cons are primitive procedures -> no thunk here

;; 4) Cy's method for practical reasons

;; Excercise 4.31
;; TODO This surely will take lot of time, skipping for now.

;; Excercise 4.32
;; TODO

;; Excercise 4.33
;; TODO

;; Excercise 4.34
;; TODO
