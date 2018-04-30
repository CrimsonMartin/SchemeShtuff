;designed for R5S5 scheme
(load "errorHelpers.scm")
(load "carcdrHelpers.scm")


;------------------------
; Environment/State Functions
;------------------------
; state is defined as ((class1) (class2) ... )
; where each class is: (classname parentclass (instancefields) (staticfields) (instance functions) (static functions) (constructors))
; fields are stored as ((vars) (vals))
; functions are stored as:(fname parent (parameters) (body) (bindings))
; and instance functions is: (function1 function2 ...)
; accessing this stuff is in carcdrHelpers.scm


; our thing works on a stack and state system: ((stack)(state))
; where the stack is where you push and pop function frames, as need in try/catch, function execution, instances, etc.
; and state is where we declare all the classes and their stuff, as well as functions in each class, etc.
; as a whole, this is called the environment


; return the environment with a frame removed from the stack
(define (pop-frame env) (list (remainingframes (stack env)) (state env)))
(define (push-frame env) (list (add-frame (new-stack-frame) (stack env)) (state env)))
; push a given frame onto the stack
(define (add-instance-frame frame env) (list (add-frame frame (stack env)) (state env)))




; some abstractions
(define (add-frame newframe l) (cons newframe l))

(define (new-bindings) '(()()))
(define (new-environment) '(()()))
(define (new-stack-frame) '(()()))

(define (add-pair var val  bindings)
(list (cons var (variables bindings)) (cons val (vals bindings))))


(define (new-class name parent instancefields staticfields instancefunctions staticfunctions constructors)
(list name parent instancefields staticfields instancefunctions staticfunctions constructors)

(define (new-function name parent params body bindings)
(list name parent params body bindings))


; create the new class and add it to the state
(define (add-class name parent instancefields staticfields instancefunctions staticfunctions constructors state)
(add-frame (list new-class name parent instancefields staticfields instancefunctions staticfunctions constructors) state)

(define (add-function name parent params body bindings classfunctions)
(add-frame (list new-function name parent params body bindings) classfunctions))


(define (is-static-vbl? var cframe)
(exists-in-list? var (vars (class-static-fields cframe))))

(define (is-static-fn? fname cframe)
(null? (get-function-from-frame fname (class-static-functions cframe))))


; returns formal binding of the function fname in the given class in the environment
(define (get-function fname class env)
(cond
  ((not (null? (get-function-from-frame fname (class-instancefunctions (get-class class env)))))
    (get-function-from-frame fname (class-instancefunctions (get-class class env))))
  ((not (null? (get-function-from-frame fname (class-staticfunctions (get-class class env)))))
    (get-function-from-frame fname (class-staticfunctions (get-class class env))))
  (else (myerror "error: function undefined- " fname))))

(define (get-function-from-frame fname classfunctions)
(cond
  ((null? classfunctions) NULL)
  ((eq? fname (function-name (top-frame state))) (top-frame classfunctions))
  (else (get-function-from-frame fname (remaining-frames classfunctions)))))


; returns the formal binding of the class in the environment
(define (get-class cname env)
(cond
  ((null? (get-class-from-state cname (state env))) (myerror "error: class not defined- " cname))
  (else (get-class-from-state cname (state env)))))

(define (get-class-from-state cname state)
(cond
  ((null? state) NULL)
  ((eq? cname (class-name (top-frame state))) (top-frame state))
  (else (get-class-from-state cname (remaining-frames state)))))


;used to reconstruct the state and function frames
(define (get-all-other-function fname classfunctions)
(cond
  ((null? classfunctions) NULL)
  ((equal? fname (function-name (top-frame classfunctions)))
    (remaining-frames classfunctions))
  (else (cons (top-frame classfunctions) (get-all-other-function fname (remaining-frames classfunctions))))))

(define (get-all-other-class cname state)
(cond
  ((null? state) NULL)
  ((equal? cname (class-name (top-frame state)))
    (remaining-frames state))
  (else (cons (top-frame state) (get-all-other-class cname (remaining-frames state))))))

;returns the new env
(define (replace-function oldfunction-name newfunction-frame classname env)
(list (stack env) (add-frame (replace-function-in-class oldfunction-name newfunction-frame (get-class classname env))
  (get-all-other-class classname (state env)))))

;returns the new class frame
(define (replace-function-in-class oldfunction-name newfunction-frame class-frame)
(cond
  ((is-static-fn? oldfunction-name class-frame) (add-frame newfunction-frame
    (get-all-other-function oldfunction-name (class-static-functions class-frame))))
  (else (add-frame newfunction-frame (get-all-other-function oldfunction-name (class-instance-functions class-frame))))))


(define (replace-class oldclass-name newclass-frame env)
(list (stack env) (replace-class-in-state oldclass-name newclass-frame (state env))))

(define (replace-class-in-state oldclass-name newclass-frame state)
(addframe newclass-frame (get-all-other-class oldclass-name state)))











;needs to look in the function instance, then class instance, then parent function instances, then in the class instancefields, then class static fields
; in that order
(define (exists? var function class env)
(cond
  ((
  ((exists-in-function? var (get-function function class env)) #t)
  ;checks class instance fields then class static fields
  (else (exists-in-class? var (get-class class env)))


(define (exists-in-function? var fframe)
(exists-in-list? var (vars (function-bindings fframe))))

(define (exists-in-class? var cframe)
(or (exists-in-list? var (vars (class-instance-fields cframe)))
    (exists-in-list? var (vars (class-static-fields cframe)))))



; Looks up a value in the environment - entry point function
; Returns an error if the variable does not have a legal value
(define (lookup var function class env)
    (if (not (exists? var function class env))
      (myerror "error: undefined variable: " var)
      (lookup-in-env var function class state)))


;first check the
(define (lookup-in-env var function class state)
(cond
  ((exists-in-function? var (get-function function (get-class class state)))
    (lookup-in-function var (get-funciton function (get-class class state))))
  ((exists-in-class? var (get-class class state)) (lookup-in-class var (get-class class state)))
  (else

(define (lookup-in-class var cframe)
(cond
  ((exists-in-list? var (class-instancefields cframe))
    (get-value (indexof var (class-instancefields cframe)) (class-instancefields cframe)))
  (else (get-value (indexof var (class-staticfields cframe)) (class-staticfields cframe)))))


(define (lookup-in-function-parent var function class state)
(lookup var (function-parent (get-function function (class-functions (get-class class state)))) class state))

(define (lookup-in-class-parent var function class state)
(lookup var function (class-parent (get-class class state)) state))


; looks up all the values in the list, and returns a list of values
; ex ((x y z) (1 2 3)) is the bound variables
; input (x y z) returns (1 2 3)
(define (lookup-list list class state)
  (cond
    ((null? list) '())
    (else (cons (lookup var class state) (lookup-list (cdr list) class state)))))




; Adds a new (var, val) binding pair into the function defined in fname
; if we're defining a global variable, put fname = 'global
(define (insert-binding var val fname class state)
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


(define (insert-binding-list newbindings fname state)
  (cond
    ((null? newbindings) state)
    (else insert-binding-list (list (cdr (variables newbindings)) (cdr (vals newbindings))) fname
      (insert-binding (car (variables newbindings)) (car (vals newbindings)) fname state))))
