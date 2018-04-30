;designed for R5S5 scheme
(load "errorHelpers.scm")
(load "carcdrHelpers.scm")


;------------------------
; Environment/State Functions
;------------------------


; some abstractions
(define (add-frame newframe state) (cons newframe state))

(define (pop-function-frame environment) (remaining-frames environment))
(define (push-frame environment) (push-function-frame 'block environment))

(define (new-bindings) '(()()))
(define (new-environment) '())

(define (add-pair var val  bindings)
(list (cons var (variables bindings)) (cons val (vals bindings))))


(define (new-class name parent instancefields staticfields functions constructors)
(list name parent instancefields staticfields functions constructors)

(define (new-function name params body bindings)
(list name params body bindings))


(define (exists-in-function? var fframe)
(exists-in-list? var (variables (function-bindings fframe))))

(define (exists-in-class? var classname cframe)
(exists-in-list? var (class-instancefields  cframe)))


(define (get-function fname classfunctions)
(cond
  ((null? classfunctions) (myerror "error: function not defined- " functionname))
  ((equal? fname (function-name (top-frame state))) (top-frame classfunctions))
  (else (get-function fname (remaining-frames classfunctions)))

(define (get-class cname state)
(cond
  ((null? state) (myerror "error: class not defined- " cname))
  ((eq? (class-name (top-frame state)) cname) (top-frame state))
  (else (get-class cname (remaining-frames state)))))

;used to reconstruct the state and function frames
(define (get-all-other-function fname classfunctions)
(cond
  ((null? classfunctions) (myerror "error: function not defined- " fname))
  ((equal? fname (function-name (top-frame classfunctions)))
    (remaining-frames classfunctions))
  (else (cons (top-frame classfunctions) (get-all-other-function fname (remaining-frames classfunctions))))))

(define (get-all-other-class cname state)
(cond
  ((null? state) (myerror "error: class not defined- " cname))
  ((equal? cname (class-name (top-frame state)))
  (remaining-frames state))
  (else (cons (top-frame state) (get-all-other-class cname (remaining-frames state))))))


(define (replace-function oldfunction-name newfunction class-frame)
(append (newfunction (get-all-other-function oldfunction-name (class-functions class-frame)))))

(define (replace-class oldclass-name newclass state)
(append (newclass (get-all-other-class oldclass-name state))))




; EVERYTHING BELOW HERE NEEDS TO BE REWORKED





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



; Looks up a value in the environment
; Returns an error if the variable does not have a legal value
;(define (lookup var class state)
;    (if (not (exists? var class environment))
;        (myerror "error: undefined variable: " var)
;        (lookup-in-env var class environment)))
;
; Return the value bound to a variable in the environment
;(define (lookup-in-env var class environment)
;  (if (exists-in-frame? var (get-function fname environment))
;      (lookup-in-frame var (get-function fname environment))
;      (lookup-in-env var (function-parent (get-function fname environment)) environment) ))

; Return the value bound to a variable in the funtion
;(define (lookup-in-function var functionframe)
;  (language->scheme (get-value (indexof var (variables (function-bindings functionframe))) (vals (function-bindings functionframe)))))
