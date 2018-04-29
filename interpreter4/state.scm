;designed for R5S5 scheme
(load "mistHelpers.scm")

; some abstractions
(define (add-frame newframe state) (cons newframe state))
(define top-frame car)
(define remaining-frames cdr)

(define (new-bindings) '(()()))
(define (variables bindings) (car bindings))
(define (vals bindings) (cadr bindings))
(define (add-pair var val  bindings)
(list (cons var (variables bindings)) (cons val (vals bindings))))



; class: (classname parentclass (instancefields) (functions) (constructors))
(define (class-name frame) (car frame))
(define (class-parent frame) (cadr frame))
(define (class-instancefields frame) (caddr frame))
(define (class-functions frame) (cadddr frame))
(define (class-constructors frame) (cadddr (cdr frame)))

(define (new-class name parent instancefields funcitons constructors)
(list name parent instancefields funcitons constructors))


; checks in the class frame first, then parent
(define (exists-in-class? var classname state)
(if (null? state) #f
  (exists-in-list? var (class-instancefields (get-class classname state)))))


; function: (name (parameters) (body) (closure))
(define (function-name closure) (car closure))
(define (function-parameters closure)(cadr closure))
(define (function-body closure)(caddr closure))
(define (function-bindings closure)(cadddr closure))

(define (new-function name params body bindings)
(list name params body bindings))

;is the var defined in a function
(define (exists-in-function? var frame)
  (exists-in-list? var (variables (function-bindings frame))))





; Looks up a value in the environment
; Returns an error if the variable does not have a legal value
(define (lookup var class state)
    (if (not (exists? var class environment))
        (myerror "error: undefined variable: " var)
        (lookup-in-env var class environment)))

; Return the value bound to a variable in the environment
(define (lookup-in-env var class environment)
  (if (exists-in-frame? var (get-function fname environment))
      (lookup-in-frame var (get-function fname environment))
      (lookup-in-env var (function-parent (get-function fname environment)) environment) ))

; Return the value bound to a variable in the funtion
(define (lookup-in-function var functionframe)
  (language->scheme (get-value (indexof var (variables (function-bindings functionframe))) (vals (function-bindings functionframe)))))
