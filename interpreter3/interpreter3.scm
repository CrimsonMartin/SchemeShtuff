;Authors:
;Michael Folz maf152
;Mark Gross mag210

;Based on the given solution interpreter2-callcc-no-boxes.scm

(load "functionParser.scm")




;TODO delete these        ( () () )
(define test-environment '((global () () ((a b c d x) (1 2 3 4 5)))))
(define test2 '((factorial global (p1)(hotbody1)((m n o p) (9 8 7 6))) (global global-parent (p2) (hotbody2) ((a b c d x) (1 2 3 4 5)))))



; An interpreter for the c-ish language that uses call/cc for the continuations.  Does not handle side effects.

; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define (interpret file)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-statement-list (interpret-functions (parser file) (new-environment) (new-bindings)) return; TODO change to eval-main as last step
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown")))))))

; reads through the methods and binds all the functions and their closures in the state
; returns the resulting state
; the environment from each step is used by the next one
(define (interpret-functions input environment current-bindings)
  (cond
    ((null? input) environment)
    ((eq? 'var (statement-type input)) ) ;bind global variable
    ((equal? 'function (statement-type input)) ;function decleration
      ;return an environment with the function bound- cdr input is everything but 'function
      (interpret-bind-function (cdr input) environment current-bindings))
    (else interpret-functions (cdr input) (interpret-functions (car input) environment current-bindings))))

(define (interpret-bind-function function-statement state current-bindings)
  (interpret-bind-function-parts (function-name function) (function-parameters function) (function-body function) ))

; stores the closure of the function in the current state
; returns the updated state
;TODO change this to insert for error-checking
(define (interpret-bind-function-parts pname paramList fBody current-bindings cstate)
    (append (list pname paramList fBody current-bindings cstate)))

; state already has main declared in body
; evaluates for the return value of main function
; TODO check that it's the right return continuation- each function creates their own return continuation
(define (eval-main state return break continue throw)
  (interpret-statement-list (function-body (get-function 'main state)) state return break continue throw))







; returns the function environment, with variables evaluated in the state
; ex (x y z) for actual-params params returns ((x y z)(1 2 3))+ rest of function environment
(define (add-bindings formal-params actual-params function-environment state)
  (if (not(is-compatible-param-list formal-param actual-param state))
      (myerror "parameter mismatch : formal parameters should be " formal-params)
      (add-param-bindings formal-params actual-params function-environment state)))

;add-param-bindings (insert (get-firstelement f-param) (eval-expression (get-firstelement a-params) state) environment) state))))




; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list pname environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) pname environment return break continue throw) return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement pname environment return break continue throw)
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
      ((eq? 'funcall (statement-type statement)) (interpret-funcall statement pname environment return break comtinue throw));TODO this needs to be a new return continuation I think
      (else (myerror "Unknown statement:" (statement-type statement))))))

(define (interpret-funcall statement pname environment return break continue throw)
  (3;TODO does the stuff from the notes
    ))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement pname environment return)
    (return (eval-expression (get-expr statement) pname environment)))) ;TODO change the return continuation to point outside the function rather than overall

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define (interpret-declare statement pname environment)
    (if (exists-declare-value? statement)
        (insert-binding (get-declare-var statement) (eval-expression (get-declare-value statement) environment) pname environment)
        (insert-binding (get-declare-var statement) 'novalue pname environment)))

; Updates the environment to add an new binding for a variable
(define (interpret-assign statement pname environment)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment) pname environment))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define (interpret-if statement pname environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment) (interpret-statement (get-then statement) pname environment return break continue throw))
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
    (pop-frame (interpret-statement-list (cdr statement) ;TODO this can't just make a new block
                                         pname
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define (interpret-throw statement pname environment throw)
    (throw (eval-expression (get-expr statement) pname environment) environment))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
;TODO figgure this out, no clue what's happening here
(define (create-throw-catch-continuation catch-statement pname environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw))))
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list
                                                 (get-body catch-statement)
                                                 (insert-binding (catch-var catch-statement) ex (push-frame env))
                                                 return
                                                 (lambda (env2) (break (pop-frame env2)))
                                                 (lambda (env2) (continue (pop-frame env2)))
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement pname environment return break continue throw)
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
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define (make-try-block try-statement)
    (cons 'begin try-statement))

(define (make-finally-block finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement)))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define (eval-expression expr pname environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr pname environment))
      ;TODO add function evaluation
      (else (eval-operator expr pname environment))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define (eval-operator expr pname environment)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) pname environment)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) pname environment)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) pname environment) pname environment))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define (eval-binary-op2 expr op1value pname environment)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) pname environment)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) pname environment)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) pname environment)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) pname environment)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) pname environment)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) pname environment)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) pname environment))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) pname environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) pname environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) pname environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) pname environment)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) pname environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) pname environment)))
      (else (myerror "Unknown operator:" (operator expr)))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


(define get-firstelement operator)
(define get-secondelement operand1)

;------------------------
; Environment/State Functions
;------------------------

; closure is of the form:
;((function name)(parameter list)(function body)(state when the function was bound))
; environment is of the form:
;((function1 closure) (function2 closure) ... (global variables))

;global variables are variables bound in the "function" named 'global


; some abstractions
(define top-frame car)
(define remaining-frames cdr)

; abstractions for the dealing with closures
(define (function-name closure)
  (car closure))
(define (function-parent closure)
  (cadr closure))
(define (function-parameters closure)
  (caddr closure))
(define (function-body closure)
  (cadddr closure))
(define (function-bindings closure)
  (car (cddr (cddr closure))))

(define (variables frame)
  (car frame))
(define (vals frame)
  (cadr frame))

(define (new-bindings)
  '(()()))

; creates a new function frame
(define (new-function-frame fname)
  (list fname 'global-parent '() '() (new-bindings)))

; create a new empty environment, with function name 'global
(define (new-environment)
  (list (new-function-frame 'global)))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

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

; does a variable exist in the environment?(begin (if (null? input)
; checks in the pname frame first, then parent up the parent function list
(define (exists? var pname environment)
    (cond
      ;if we're on global (base) function frame, return if it's a global vbl or not
      ((equal? 'global (function-name (get-function pname environment))) (exists-in-frame? var (get-function 'global environment)))
      ((exists-in-frame? var (get-function pname environment)) #t)
      ;else recurse on the parent function frame
      (else (exists? var (function-parent (get-function pname environment)) environment))))

;is the var defined in a function frame
(define (exists-in-frame? var frame)
  (exists-in-list? var (variables (function-bindings frame))))


; looks up all the values in the list, and returns a list of values
; ex ((x y z) (1 2 3) is the bound variables in the state
; input (x y z) returns (1 2 3)
(define (lookup-list list pname state)
  (cond
    ((null? list) '())
    (else (cons (lookup var pname environment) (lookup-list (cdr list) pname state)))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
; first it looks in the given function frame, then in the parents up the parent tree to global
; Returns an error if the variable does not have a legal value
(define (lookup var pname environment)
    (if (not (exists? var pname environment))
        (myerror "error: undefined variable: " var)
        (lookup-in-env var pname environment))) ;else return the value

; Return the value bound to a variable in the environment
(define (lookup-in-env var pname environment)
  (if (exists-in-frame? var (get-function pname environment))
      (lookup-in-frame var (get-function pname environment))
      (lookup-in-env var (function-parent (get-function pname environment)) environment) ))

; Return the value bound to a variable in the frame
(define (lookup-in-frame var frame)
  (language->scheme (get-value (indexof var (variables (function-bindings frame))) (vals (function-bindings frame)))))

; Adds a new (var, val) binding pair into the function defined in pname
; if we're defining a global variable, put pname = 'global
(define (insert-binding var val pname state)
    (if (exists-in-frame? var (get-function pname state))
        (myerror "error: variable is being re-declared:" var)
        (replace-function pname (replace-bindings (get-function pname state) (insert-in-frame var val (get-function pname state))) state)))

;insert the var val pair into the given function frame
(define (insert-in-frame var val frame)
  (list (cons var (variables (function-bindings frame)))
        (cons (scheme->language val) (vals (function-bindings frame)))))

; Changes the binding of a variable to a new value in the environment
; gives an error if the variable does not exist already
; to change global variable, put 'global as the pname
; returns the new state with this updated
; TODO mark check 
(define (update var val pname state)
    (cond
      ;base case- global variables
      ((and (equal? pname 'global)
            (exists-in-frame? var (get-function 'global state))) (update-in-frame var val (get-function 'global state)))
      ((not (exists? var pname state))
        (myerror "error: variable used but not defined:" var))
      ((exists-in-frame? var (get-function pname state))
        (replace-function pname (update-in-frame var val (get-function pname state)) state))
      (else (cons (get-function pname state) (update var val (function-parent (get-function pname state)) state)))))


; Changes the binding of a variable in the frame to a new value
; returns the updated frame
(define (update-in-frame var val frame)
  (replace-bindings frame (replace-varval-pair var val (variables (function-bindings frame)) (vals (function-bindings frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the values
; returns the new updated bindings
(define (replace-varval-pair var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (list varlist (cons (scheme->language val) (cdr vallist))))
      (else (add-pair (car varlist) (car vallist) (replace-varval-pair var val (cdr varlist) (cdr vallist))))))

;TODO check if this can be re-used anywhere to clean up stuff
(define (add-pair var val  bindings)
  (list (cons var (variables bindings)) (cons val (vals bindings))))

; helper for insert to reconstruct the state after insertion
(define (replace-bindings frame newbindings)
  (list (function-name frame) (function-parent frame) (function-parameters frame) (function-body frame) newbindings))

;helper for insert to reconstruct the state after insertion
(define (replace-function old-function-name new-frame state)
  (cond
    ((equal? old-function-name (function-name (top-frame state)))
     (cons new-frame (remaining-frames state)))
    (else (cons (top-frame state) (replace-function old-function-name new-frame (remaining-frames state))))))

; adds the list of bindings to the given function in the state
; binding list is ((vars)(vals))
; returns the overall state
(define (insert-binding-list newbindings pname state)
  (cond
    ((null? newbindings) state)
    (else insert-binding-list (list (cdr (variables newbindings)) (cdr (vals newbindings))) pname
      (insert-binding (car (variables newbindings)) (car (vals newbindings)) pname state))))



;global is the function name in the last frame of the state, so same as getting the function 'global
(define (get-global-closure state)
  (get-function 'global state))


;lookup a function from the state, returns the closure
(define (get-function fname state)
  (cond
    ((null? state) (myerror "error: couldn't find function " pname))
    ((equal? fname (function-name (top-frame state))) (top-frame state))
    ;I chose equal so that functions are caps independant, if we want f() and F() to be considered the same we need to manipulate the atom fname
    (else (get-function fname (remaining-frames state)))))


;checks that l2 is a viable input to the function with formal params l1
(define (is-compatible-param-list l1 l2 state)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f); if we get here, then one is not null so the length is mismatched
      ;this is here so it returns and doesn't crash
    ((not (is-compatible? (car l1) (car l2) state)) #f)
    (else (is-compatible-param-list (cdr l1) (cdr l2) state))))

(define (is-compatible? x y state)
    ;TODO not sure if we need to check for compatibility?
  ;needs y needs to be evaluate
    #t)














































; Miscelaneous helper functions

(define call/cc call-with-current-continuation)

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v)
    (cond
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))
