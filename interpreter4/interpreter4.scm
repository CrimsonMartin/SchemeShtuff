;Authors:
;Michael Folz maf152
;Mark Gross mag210
;Andrew Mooney ajm230

;Based on the given solution interpreter2-callcc-no-boxes.scm
(load "classParser.scm")

;for handling the state stuff, also loads the carcdrHelpers and the errorHelpers
(load "stateHelpers.scm")



; An interpreter for the java-ish language that uses call/cc for the continuations.  Does not handle side effects.

; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

(define (interpret file runclass)
  (scheme->language
   (call/cc
     (lambda (return)
       (eval-main (interpret-classes (parser file) (new-environment)) return
          (lambda (env) (myerror "Break used outside of loop"))
          (lambda (env) (myerror "Continue used outside of loop"))
          (lambda (v env) (myerror "Uncaught exception thrown"))) ))))


(define (interpret-classes input env)
(cond
  ((null? input) env)
  ((eq? 'class (operator input))
    (if (exists-operand2? input);we are extending a class
      (interpret-declare-class (operand1 input) (operand1 (operand2 input)) (operand3 input) env)
      ;else we're not extending a class - denote this by parent function called 'mainparent
      (interpret-declare-class (opeand1 input) 'mainparent (opernad3 input) env)))
  (else (interpret-classes (cdr input) (interpret-classes (car input) env)))))

;returns the env after declaring the class in the env
(define (interpret-declare-class name parent body env)
(list (stack env)
(add-class name parent
  (parse-instancefields body)
  (parse-staticfields body)
  (parse-instancefunctions body)
  (parse-staticfunctions)
  (parse-constructors body) (state env) )))



;takes the body of a class and breaks it down into it's pieces for easier acccess
(define (parsehelper stmt-type body)
(cond
  ((null? body) (new-bindings))
  ((eq? stmt-type (statement-type (car body))) (cons (car body) (parsehelper (cdr body))))
  (else (parsehelper (cdr body)))))



;takes the body of a class and breaks it down into it's pieces for easier acccess
(define (parse-instancefields body)
(parsehelper 'var body))

;assuming that static fields can't be initialized to 'novalue (since we don't have fancy constructors, they would never get initialized)
(define (parse-staticfields body)
(parsehelper 'static-var body))

(define (parse-instancefunctions body)
(parsehelper 'function body))

(define (parse-staticfunctions body)
(parsehelper 'static-function body))

(define (parse-constructors function class body)
;change this if we decide to implement strange constructors
  (no-constructors))


; state already has main declared in body
; evaluates for the return value of main function
(define (eval-main class env return break continue throw)
  (interpret-statement-list (function-body (get-function 'main (get-class class env)))
  class env return break continue throw))

  

; returns the function environment, with variables evaluated in the state
; ex (x y z) for actual-params params returns ((x y z)(1 2 3))+ rest of function environment
(define (add-bindings formal-params actual-params compiletime-type function-environment state)
  (if (not(is-compatible-param-list formal-param actual-param state))
      (myerror "parameter mismatch : formal parameters should be " formal-params)
      (add-param-bindings formal-params actual-params compiletime-type function-environment state)))



; interprets a list of statements.  The environment from each statement is used for the next ones.
; pname is the name of the calling function
(define (interpret-statement-list statement-list compiletime-type environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) pname (interpret-statement (car statement-list) pname compiletime-type environment return break continue throw) return break continue throw)))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define (interpret-statement statement pname compiletime-type environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement pname compiletime-type environment return))
      ((eq? 'var (statement-type statement)) (interpret-declare statement pname compiletime-type environment))
      ((eq? '= (statement-type statement)) (interpret-assign statement pname compiletime-type environment))
      ((eq? 'if (statement-type statement)) (interpret-if statement pname compiletime-type environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement pname compiletime-type environment return throw))
      ((eq? 'continue (statement-type statement)) (continue pname compiletime-type environment))
      ((eq? 'break (statement-type statement)) (break pname compiletime-type environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement pname compiletime-type environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement pname compiletime-type environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement pname compiletime-type environment return break continue throw))
      ((eq? 'funcall (statement-type statement)) (call/cc ;whenever we're inside a funcall, return brings us back here rather than to the beginning of the function
                                                  (lambda (return) (return (interpret-funcall statement  statement compiletime-type environment return break continue throw)))))
      (else (myerror "Unknown statement:" (statement-type statement)))))


; when we evaluate a function, we need to pass back the parameters that were input into the function into the parent function's bindings- update them
; each time we modify variables inside, we already update them as needed to the parent functions up the chain to global variables
; so 1: evaluate the actual parameter list in the environment
; 2: pass the ((formal params)(evaluated actual params)) bindings into the bindings of the function
; 3: run the function
;     - automatically updates the variables wherever they need to be on an equals, since update goes up the call tree until the variable is defined
; 4: pass the modified function bindings back to the parent function recursively, until it updates wherever they are defined
; 5: return the value of the return continuation in the function (if it exists- if not, just do 4)
(define (interpret-funcall statement fname compiletime-type environment return break continue throw)
 ; (length (function-parameters (get-function fname compiletime-type environment)))


  ;returns the state after the function body has been executed
           (interpret-statement-list (function-body (get-funciton fname compiletime-type environment)) fname
                   (insert-binding-list (list (function-params (get-function fname compiletime-type environment))
                                              (lookup-list (function-params (get-function fname compiletime-type environment)) fname state))
                       fname compiletime-type environment)
                   return break continue throw))

; Calls the return continuation with the given expression value
(define (interpret-return statement fname compiletime-type environment return)
    (return (eval-expression (get-expr statement) fname compiletime-type environment)))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define (interpret-declare statement pname compiletime-type environment)
    (if (exists-declare-value? statement)
        (insert-binding (get-declare-var statement) (eval-expression (get-declare-value statement) pname compiletime-type environment) pname compiletime-type environment)
        (insert-binding (get-declare-var statement) 'novalue pname compiletime-type environment)))

; Updates the environment to add an new binding for a variable
(define (interpret-assign statement pname compiletime-type environment)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) pname compiletime-type environment) pname compiletime-type environment))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define (interpret-if statement pname compiletime-type environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) pname compiletime-type environment) (interpret-statement (get-then statement) pname compiletime-type environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) pname compiletime-type environment return break continue throw))
      (else environment)))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define (interpret-while statement pname compiletime-type environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body pname compiletime-type environment)
                        (if (eval-expression condition pname compiletime-type environment)
                            (loop condition body (interpret-statement body pname compiletime-type environment return break (lambda (env) (break (loop condition body env))) throw))
                         compiletime-type environment))))
         (loop (get-condition statement) (get-body statement) pname compiletime-type environment)))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define (interpret-block statement pname compiletime-type environment return break continue throw)
    (pop-function-frame (interpret-statement-list (cdr statement)
                                         pname
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-function-frame env)))
                                         (lambda (env) (continue (pop-function-frame env)))
                                         (lambda (v env) (throw v (pop-function-frame env))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define (interpret-throw statement pname compiletime-type environment throw)
    (throw (eval-expression (get-expr statement) pname compiletime-type environment) environment))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define (create-throw-catch-continuation catch-statement pname compiletime-type environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw))))
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-function-frame (interpret-statement-list
                                                 (get-body catch-statement)
                                                 (insert-binding (catch-var catch-statement) ex (push-frame env))
                                                 return
                                                 (lambda (env2) (break (pop-function-frame env2)))
                                                 (lambda (env2) (continue (pop-function-frame env2)))
                                                 (lambda (v env2) (throw v (pop-function-frame env2)))))
                                     return break continue throw))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define (interpret-try statement pname compiletime-type environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block pname compiletime-type environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block pname env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block pname env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) pname compiletime-type environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block pname compiletime-type environment new-return new-break new-continue new-throw)
                          return break continue throw)))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define (make-try-block try-statement)
    (cons 'begin try-statement))

(define (make-finally-block finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement)))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define (eval-expression expr fname compiletime-type environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr fname compiletime-type environment))
      (else (eval-operator expr fname compiletime-type environment))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define (eval-operator expr fname compiletime-type environment)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) fname compiletime-type environment)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) fname compiletime-type environment)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) fname compiletime-type environment) fname compiletime-type environment))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define (eval-binary-op2 expr op1value fname compiletime-type environment)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) fname compiletime-type environment))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) fname compiletime-type environment)))
      (else (myerror "Unknown operator:" (operator expr)))))
