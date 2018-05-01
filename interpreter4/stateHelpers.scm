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
(define (top-stack-frame env) (top-frame (stack env)))
(define (push-frame env) (list (add-frame (new-stack-frame) (stack env)) (state env)))
; push a given frame onto the stack
(define (add-instance-frame frame env) (list (add-frame frame (stack env)) (state env)))


; some abstractions
(define (add-frame newframe l) (cons newframe l))

(define (new-bindings) '(()()))
(define (new-environment) '(()()))
(define (new-stack-frame) '(class type ()()))

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


; Looks up a value in the environment - entry point function
; Returns an error if the variable does not have a legal value
(define (lookup var function class env)
  (if (not (exists? var function class env))
    (myerror "error: undefined variable: " var)
    (lookup-in-env var function class state)))

(define (lookup-in-env var function class env)
(cond
  ((not (null? (lookup-instance var function class env)))
    (lookup-instance var function class env))
  ((not (null? (lookup-static var function class env)))
    (lookup-static var function class env))
  (else NULL)))

;looks up the value of the var, is non-static variable
(define (lookup-instance var function class env)
(cond
  ((not (null? (lookup-in-frame var (top-stack-frame env))))
    (lookup-in-frame var (top-stack-frame env)))
  ((eq? 'mainparent (class-parent (get-class class env)))
    (lookup-instance-class (class-instancefields (get-class class env))))
  ((not (null? (lookup-instance-class var (class-instancefields (get-class class env)))))
    (lookup-instance-class var (class-instancefields (get-class class env))))
  (else (lookup-instance var function (class-parent (get-class class env)) env))))


(define (lookup-instance-class var classbindings)
(lookup-in-frame var classbindings))

(define (lookup-in-frame var stackframe)
(cond
  ((not (zero? (indexof var (frame-vars stackframe))))
    (get-value (indexof var (frame-vars stackframe)) (frame-vals stackframe)))
  (else NULL)))


;looks up the value of the var, is static variable
(define (lookup-static var function class env)
(cond
  ((eq? 'mainparent (class-parent (get-class class env))) NULL)
  ((exists-in-class? var (get-class class env)) (lookup-in-class-static var class env))
  (else (lookup-static var function (class-parent (get-class class env)) env))))

(define (lookup-in-class-static var class env)
(get-value (indexof var (vars (class-staticfields (get-class class env))))
  (vals (class-staticfields (get-class class env)))))


;needs to look in the function instance, then class instance, then parent function instances, then in the class instancefields, then class static fields
; in that order
(define (exists? var function class env)
(cond
  ((not (null? (lookup-instance var function class env))) #t)
  ((not (null? (lookup-static var function class env))) #t)
  (else #f)))


; looks up all the values in the list, and returns a list of values
; ex ((x y z) (1 2 3)) is the bound variables
; input (x y z) returns (1 2 3)
(define (lookup-list list function class env)
  (cond
    ((null? list) NULL)
    (else (cons (lookup (car list) function class env)
      (lookup-list (cdr list) function class env)))))


;gets the function's compile time type given function in the environment
;reads through the stack until it finds a class frame, then it gets the type of that class
;used for instantiating this. in function calls
(define (get-runtype env)
(cond
  ((null? (stack env)) NULL)
  ((is-classframe? (top-stack-frame env)) (frametype (top-stack-frame env)))
  (else (get-runtype (pop-frame env))))

(define (is-classframe? stackframe)
  (eq? 'class (frametype stackframe)))
