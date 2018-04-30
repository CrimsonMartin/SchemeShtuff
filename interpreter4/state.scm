;designed for R5S5 scheme
(load "errorHelpers.scm")
(load "carcdrHelpers.scm")
(load "errorHelpers.scm")



;------------------------
; Environment/State Functions
;------------------------


; some abstractions
(define (add-frame newframe state) (cons newframe state))

(define (new-bindings) '(()()))

(define (add-pair var val  bindings)
(list (cons var (variables bindings)) (cons val (vals bindings))))

; Changes a variable binding by placing the new value in the appropriate place in the values
; returns the new updated bindings
(define (replace-varval-pair var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (list varlist (cons (scheme->language val) (cdr vallist))))
      (else (add-pair (car varlist) (car vallist) (replace-varval-pair var val (cdr varlist) (cdr vallist))))))



(define (new-class name parent instancefields funcitons constructors)
(list name parent instancefields funcitons constructors)

(define (new-function name params body bindings)
(list name params body bindings))


(define (exists-in-function? var fframe)
(exists-in-list? var (variables (function-bindings fframe))))

(define (exists-in-class? var classname cframe)
(exists-in-list? var (class-instancefields  cframe)))


(define (get-class cname state)
(cond
  ((null? state) (myerror "error: class not defined- " cname))
  ((eq? (class-name (top-frame state)) cname) (top-frame state))
  (else (get-class cname (remaining-frames state)))))

(define (get-function fname classfunctions)
(cond
  ((null? classfunctions) (myerror "error: function not defined- " functionname))
  ((equal? fname (function-name (top-frame state))) (top-frame classfunctions))
  (else (get-function fname (remaining-frames classfunctions)))


;how to reconstruct the state and function frames
(define (get-all-other-class cname state)
(cond
  ((null? state) (myerror "error: class not defined- " cname))
  ((equal? cname (class-name (top-frame state)))
    (remaining-frames state))
  (else (cons (top-frame state) (get-all-other-class cname (remaining-frames state))))))

(define (get-all-other-function fname classfunctions)
(cond
  ((null? classfunctions) (myerror "error: function not defined- " fname))
  ((equal? fname (function-name (top-frame classfunctions)))
    (remaining-frames classfunctions))
  (else (cons (top-frame classfunctions) (get-all-other-function fname (remaining-frames classfunctions))))))


(define (replace-function oldfunction-name newfunction class-frame)
()

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
