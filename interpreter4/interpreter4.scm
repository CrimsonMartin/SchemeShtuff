;Authors:
;Michael Folz maf152
;Mark Gross mag210
;Andrew Mooney ajm230

;Based on the given solution interpreter2-callcc-no-boxes.scm
(load "classParser.scm")

;for handling the state stuff, also loads the carcdrHelpers and the errorHelpers
(load "state.scm")



; An interpreter for the java-ish language that uses call/cc for the continuations.  Does not handle side effects.

; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

(define (interpret file runclass)
  (scheme->language
   (call/cc
     (lambda (return)
       (eval-main (interpret-functions (parser file) (new-environment)) return
                              (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown"))) ))))



(define (interpret-classes input state)
(cond
  ((null? input) state)
  ((eq? (car input) 'class ) (;declare a class))))
  (else (interpret-classes (cdr input) (interpret-classes (car input) state))))











; reads through the methods and binds all the functions and their closures in the state
; returns the resulting state
; the environment from each step is used by the next one
(define (interpret-functions input environment)
  (cond
    ((null? input) environment)
    ((and (eq? 'var (statement-type input)) (not (exists-operand2? input))); (var x)
     (insert (operand1 input) 'novalue 'global environment))
    ((and (eq? 'var (statement-type input)) (exists-operand2? input)); (var x 10)
     (insert-binding (operand1 input) (operand2 input) 'global environment))
    ((equal? 'function (statement-type input)) ;function decleration
      (interpret-bind-function (cdr input) environment))
    ((list? (car input)) (interpret-functions (cdr input) (interpret-functions (car input) environment)  ))
    (else (myerror "illegal global declaration"))))

(define (interpret-bind-function function state)
  (interpret-bind-function-parts (function-formal-name function) (function-formal-params function) (function-formal-body function) state))


; stores the closure of the function in the current state
; returns the updated state
(define (interpret-bind-function-parts fname paramList fbody state)
    (cons (list fname 'global paramList fbody (new-bindings)) state))

; state already has main declared in body
; evaluates for the return value of main function
(define (eval-main state return break continue throw)
  (interpret-statement-list (function-body (get-function 'main state)) 'global state return break continue throw))



; returns the function environment, with variables evaluated in the state
; ex (x y z) for actual-params params returns ((x y z)(1 2 3))+ rest of function environment
(define (add-bindings formal-params actual-params function-environment state)
  (if (not(is-compatible-param-list formal-param actual-param state))
      (myerror "parameter mismatch : formal parameters should be " formal-params)
      (add-param-bindings formal-params actual-params function-environment state)))



; interprets a list of statements.  The environment from each statement is used for the next ones.
; pname is the name of the calling function
(define (interpret-statement-list statement-list pname environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) pname (interpret-statement (car statement-list) pname environment return break continue throw) return break continue throw)))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define (interpret-statement statement pname environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement pname environment return))
      ((eq? 'var (statement-type statement)) (interpret-declare statement pname environment))
      ((eq? '= (statement-type statement)) (interpret-assign statement pname environment))
      ((eq? 'if (statement-type statement)) (interpret-if statement pname environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement pname environment return throw))
      ((eq? 'continue (statement-type statement)) (continue pname environment))
      ((eq? 'break (statement-type statement)) (break pname environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement pname environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement pname environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement pname environment return break continue throw))
      ((eq? 'funcall (statement-type statement)) (call/cc ;whenever we're inside a funcall, return brings us back here rather than to the beginning of the function
                                                  (lambda (return) (return (interpret-funcall statement  statement environment return break continue throw)))))
      (else (myerror "Unknown statement:" (statement-type statement)))))


; when we evaluate a function, we need to pass back the parameters that were input into the function into the parent function's bindings- update them
; each time we modify variables inside, we already update them as needed to the parent functions up the chain to global variables
; so 1: evaluate the actual parameter list in the environment
; 2: pass the ((formal params)(evaluated actual params)) bindings into the bindings of the function
; 3: run the function
;     - automatically updates the variables wherever they need to be on an equals, since update goes up the call tree until the variable is defined
; 4: pass the modified function bindings back to the parent function recursively, until it updates wherever they are defined
; 5: return the value of the return continuation in the function (if it exists- if not, just do 4)
(define (interpret-funcall statement fname environment return break continue throw)
 ; (length (function-parameters (get-function fname environment)))


  ;returns the state after the function body has been executed
           (interpret-statement-list (function-body (get-funciton fname environment)) fname
                   (insert-binding-list (list (function-params (get-function fname environment))
                                              (lookup-list (function-params (get-function fname environment)) fname state))
                       fname environment)
                   return break continue throw))

; Calls the return continuation with the given expression value
(define (interpret-return statement fname environment return)
    (return (eval-expression (get-expr statement) fname environment)))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define (interpret-declare statement pname environment)
    (if (exists-declare-value? statement)
        (insert-binding (get-declare-var statement) (eval-expression (get-declare-value statement) pname environment) pname environment)
        (insert-binding (get-declare-var statement) 'novalue pname environment)))

; Updates the environment to add an new binding for a variable
(define (interpret-assign statement pname environment)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) pname environment) pname environment))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define (interpret-if statement pname environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) pname environment) (interpret-statement (get-then statement) pname environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) pname environment return break continue throw))
      (else environment)))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define (interpret-while statement pname environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body pname environment)
                        (if (eval-expression condition pname environment)
                            (loop condition body (interpret-statement body pname environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) pname environment)))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define (interpret-block statement pname environment return break continue throw)
    (pop-function-frame (interpret-statement-list (cdr statement)
                                         pname
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-function-frame env)))
                                         (lambda (env) (continue (pop-function-frame env)))
                                         (lambda (v env) (throw v (pop-function-frame env))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define (interpret-throw statement pname environment throw)
    (throw (eval-expression (get-expr statement) pname environment) environment))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define (create-throw-catch-continuation catch-statement pname environment return break continue throw jump finally-block)
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
(define (interpret-try statement pname environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block pname environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block pname env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block pname env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) pname environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block pname environment new-return new-break new-continue new-throw)
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
(define (eval-expression expr fname environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr fname environment))
      (else (eval-operator expr fname environment))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define (eval-operator expr fname environment)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) fname environment)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) fname environment)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) fname environment) fname environment))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define (eval-binary-op2 expr op1value fname environment)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) fname environment)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) fname environment)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) fname environment)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) fname environment)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) fname environment)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) fname environment)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) fname environment))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) fname environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) fname environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) fname environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) fname environment)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) fname environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) fname environment)))
      (else (myerror "Unknown operator:" (operator expr)))))



      ;------------------------
      ; Environment/State Functions
      ;------------------------

; create a new empty environment, with function name 'global
(define (new-environment)
  (list (new-function-frame 'global)))



(define (push-frame environment)
  (push-function-frame 'block environment))

; add a frame onto the top of the environment
(define (push-function-frame name environment)
    (cons (new-function-frame name) environment))

;adds the given function to the state with empty bindings
(define (add-function-frame fname parentname params body state)
  (cons (list fname parentname params body (new-bindings) state)))

; remove a frame from the environment
(define (pop-function-frame environment)
    (remaining-frames environment))


; looks up all the values in the list, and returns a list of values
; ex ((x y z) (1 2 3) is the bound variables in the state
; input (x y z) returns (1 2 3)
(define (lookup-list list fname state)
  (cond
    ((null? list) '())
    (else (cons (lookup var fname environment) (lookup-list (cdr list) fname state)))))

; Adds a new (var, val) binding pair into the function defined in fname
; if we're defining a global variable, put fname = 'global
(define (insert-binding var val fname state)
    (if (exists-in-frame? var (get-function fname state))
        (myerror "error: variable is being re-declared:" var)
        (replace-function fname (replace-bindings (get-function fname state) (insert-in-frame var val (get-function fname state))) state)))

; insert the var val pair into the given function frame
(define (insert-in-frame var val frame)
  (list (cons var (variables (function-bindings frame)))
        (cons (scheme->language val) (vals (function-bindings frame)))))


;bulk updates the ((vars )(vals)) in bindings in the fname state
;assumes it's a valid bindings
(define (update-list bindings fname state)
  (cond
    ((null? (car bindings)) state)
    (else (update (caar bindings) (cadr bindings) fname (update-list (list (cdar bindings)(cddr bindings)) fname state)))))


; Changes the binding of a variable to a new value in the environment
; gives an error if the variable does not exist already
; to change global variable, put 'global as the fname
; returns the new state with this update
; looks in the given funciton, and if the value isn't found it recurses on the parent functions until it finds the variable, and updates it
(define (update var val fname state)
  (cond
    ((null? state) (begin '()
                    (update-in-parent var val fname state)))
    ((and (exists-in-frame? var (top-frame state))
          (equal? fname (function-name (top-frame state))))
            ;we're in the right function frame
      (replace-function 'fname (update-in-frame var val (top-frame state)) state))
    (else (cons (top-frame state) (update var val fname (remaining-frames state))))))


(define (update-in-parent var val fname state)
  (cond
    ((and (equal? 'global (function-name (top-frame state)))
          (not (exists-in-frame? var (top-frame state))))
      ;we're in the global frame and the var still isn't found
      (myerror "error: variable used but not defined: " var))
    (else (update var val (funciton-parent) state))))

; Changes the binding of a variable in the frame to a new value
; returns the updated frame
(define (update-in-frame var val frame)
  (replace-bindings frame (replace-varval-pair var val (variables (function-bindings frame)) (vals (function-bindings frame)))))




; helper for insert to reconstruct the state after insertion
(define (replace-bindings frame newbindings)
  (list (function-name frame) (function-parent frame) (function-parameters frame) (function-body frame) newbindings))

;helper for insert to reconstruct the state after insertion
(define (replace-function old-function-name new-frame state)
  (cond
    ((null? state) '())
    ((equal? old-function-name (function-name (top-frame state)))
     (cons new-frame (remaining-frames state)))
    (else (cons (top-frame state) (replace-function old-function-name new-frame (remaining-frames state))))))

; adds the list of bindings to the given function in the state
; binding list is ((vars)(vals))
; returns the overall state
(define (insert-binding-list newbindings fname state)
  (cond
    ((null? newbindings) state)
    (else insert-binding-list (list (cdr (variables newbindings)) (cdr (vals newbindings))) fname
      (insert-binding (car (variables newbindings)) (car (vals newbindings)) fname state))))
