#lang sicp


;; (define (lookup-variable-value exp env)
;;   (error "Not implemented yet: LOOKUP VARIABLE VALUE"))
;; (define (make-procedure parameters body)
;;   (error "Not implemented yet: MAKE PROCEDURE"))
;; (define (primitive-procedure? procedure)
;;   (error "Not implemented yet: PRIMITIVE PROCEDURE"))
;; (define (compound-procedure? procedure)
;;   (error "Not implemented yet: COMPOUND PROCEDURE"))
;; (define (apply-primitive-procedure procedure)
;;   (error "Not implemented yet: APPLY PRIMITIVE PROCEDURE"))
;; (define (procedure-body procedure)
;;   (error "Not implemented yet: PROCEDURE BODY"))
;; (define (tend-environment parameters arguments environment)
;;   (error "Not implemented yet: EXTEND ENVIRONMENT"))
;; (define (procedure-parameters procedure)
;;   (error "Not implemented yet: PROCEDURE PARAMETERS"))

(define apply-in-underlying-scheme apply)

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;; (define (procedure-environment procedure)
;;   (error "Not implemented yet: PROCEDURE ENVIRONMENT"))

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
        ((or? exp)
         (eval (or->if exp) env))
        ((and? exp)
         (eval (and->if exp) env))
        ((let? exp)
         (eval (let->combination exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
        ((application? exp)
         (meta-apply (eval (operator exp) env)
                (list-of-values
                 (operands exp)
                 env)))
        (else
         (error "Unknown expression type: EVAL" exp))))
;; renamed from apply because of racket quirk:
;; https://stackoverflow.com/questions/7171705/how-to-run-the-metacircular-evaluator-in-drracket
(define (meta-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters
            procedure)
           arguments
           (procedure-environment
            procedure))))
        (else
         (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values
             (rest-operands exps)
             env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

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

;; Excercise 4.13
;;
;; If we would unbound variable only in the first environment where it is
;; defined, then evaluator would go for enclosing environment to check if
;; variable is defined there. So usefulness of it would be limited for only
;; "forgetting" variable in current env "from now on". But then it would be more
;; simple to just redefine it if needed.
;;
;; If we make it unbound also in enclosing environment then it
;; var can be "forgotten" for good but I don't know what use case
;; it would serve and moreover it could casue havoc in the caller
;; code.
;;
;; But first case can also wreck the caller code, since you don't know where in
;; the env stack the variable is defined. The safest option would be just to
;; allow to unbound variable in current environment.

(define (make-unbound! var env)
  (define (scan variables values)
    (cond ((null? variables) nil)
          ;; this can result in empty frame
          ;; if frame has only one binding
          ((eq? var (car variables))
           (set! variables (cdr variables))
           (set! values (cdr values)))
          (else scan
                (cdr variables)
                (cdr values))))
  (if (not (eq? env the-empty-environment))
      (let ((frame (first-frame env)))
        (let ((variables (frame-variables frame))
              (values    (frame-values frame)))
          (scan variables values)))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
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

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

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

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (eval input
                 the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; Excercise 4.16
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
(#%require racket/trace)

;; Excercise 4.16
;;
;; Implemented scan-out-defines in make-procedure. As for why I don't see other
;; benefit than the fact that with adding it do make-procedure you only need to
;; scan out defines once, while with procedure-body scan-out-defines will be
;; called each time when procedure is evaluated.

;; Excercise 4.17, 4.18
;;
;; Good explanations on SICP Solutions Wiki.
;;
;; 4.18: One definition is dependant on the other so just using let (like in the
;; excercise) will not work since dy won't be defined as variable at the time y
;; will be evaluated. Contraray doing it like in the text will work.

;; Excercise 4.19
;;
;; I would too prefer, just like Eva for definitions to be truly simultaneus. It
;; seems that would require some mechanism to build a dependency tree for
;; definitions so that they can be evaluated in order of dependecy. Of course
;; there can also be mutual (cyclic) dependencies that would need to be solved,
;; which makes whole solution not trivial.

;; Excercise 4.20
;; 1
(define (letrec? exp)
  (tagged-list? 'letrec exp))
(define (letrec->let exp)
  (define (unassigned-bindings exp)
    (map (lambda (binding) (list (car binding) ''*unassigned*)) (let-bindings exp)))
  (define (set-expressions exp)
    (map (lambda (binding) (list 'set! (car binding) (cadr binding))) (let-bindings exp)))
  (append
   (list 'let (unassigned-bindings exp))
   (set-expressions exp)
   (let-body exp)))
;; 2
;;
;; When let is expanded to lambda then lambda body will refer to symbol 'fact
;; which will not be present in procedure environment because it is not defined
;; yet, which will result in an error.

;; Excercise 4.21
;; 1
;;
;; ((lambda (n)
;;    ((lambda (fact) (fact fact n))
;;     (lambda (ft k)
;;       (if (= k 1)
;;           1
;;           (* k (ft ft (- k 1)))))))
;;  5)

;; ((lambda (n)
;;    ((lambda (fib) (fib fib n))
;;     (lambda (fib k)
;;       (if (< k 2)
;;           1
;;           (+ (fib fib (- k 1)) (fib fib (- k 2)))))))
;;  5)
;;
;; 2
;;
;; (define (f x)
;;   (define (even? n)
;;     (if (= n 0)
;;         true
;;         (odd? (- n 1))))
;;   (define (odd? n)
;;     (if (= n 0)
;;         false
;;         (even? (- n 1))))
;;   (even? x))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
         true
         (od? od? ev? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
         false
         (ev? ev? od? (- n 1))))))

;; Excercise 4.24

(define (fib n)
  (cond ((= n 1) 1)
        ((= n 2) 1)
        (else
         (+ (fib (- n 1)) (fib (- n 2))))))
(define test
  '(begin
     (define (fib n)
       (cond ((= n 1) 1)
             ((= n 2) 1)
             (else
              (+ (fib (- n 1)) (fib (- n 2))))))
     (fib 30)))

(define (timed f)
  (let ((start (runtime)))
    (display (f))
    (newline)
    (* (- (runtime) start) 0.001)))

;; (timed (lambda () (fib 30)))
;; (timed (lambda () (eval test the-global-environment)))

;; baseline
;; (timed (lambda () (fib 30)))
;; 832040
;; 34.729

;; (timed (lambda () (eval test the-global-environment)))
;; 832040
;; 13154.645

;; SICP 4.2

(define (unless condition
          usual-value
          exceptional-value)
  (if condition
      exceptional-value
      usual-value))

;; Excercise 4.25

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

;; If we attempt to evaluate `(factorial 5)` in ordinary Scheme, then we will
;; end up in infinite loop. Since in applicative-order language first all
;; arguments are evaluated, factorial function, after evaluating `unless`
;; condition will continue with evaluating `(* n (factorial (- n 1)))` going
;; into another loop cycle. Underlying `if` inside `unless` will never be called
;; evaluated.
;;
;; In normal-order language such factorial implementation should not work for
;; the same reason, but with the difference that recursive call will be
;; infinitely expanded.

;; Excercise 4.26
;; `unless` as derived expression
(define (unless-condition exp) (cadr exp))
(define (unless-usual-value exp) (caddr exp))
(define (unless-exceptional-value exp) (cadddr exp))
(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exceptional-value exp)
           (unless-usual-value exp)))

;; (define exp '(unless (= n 1)
;;     (* n (factorial (- n 1)))
;;     1))
;; (unless->if exp)
