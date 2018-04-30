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

; These helper functions are for interpreting states
(define (top-frame l) car l)
(define (remaining-frames l) cdr l)

(define (vars bindings) (car bindings))
(define (vals bindings) (cadr bindings))

(define (stack env) (car env))
(define (state env) (cadr env))

; function: (name parent (parameters) (body) (closure))
(define (function-name frame) (car frame))
(define (function-parent frame) (cadr frame))
(define (function-parameters frame)(caddr frame))
(define (function-body frame)(cadddr frame))
(define (function-bindings frame)(cadddr (cdr frame)))

(define (function-formal-name f) (car f))
(define (function-formal-params f) (cadr f))
(define (function-formal-body f) (caddr f))

; class: (classname parentclass (instancefields) (staticfields) (instance functions) (static functions) (constructors))
(define (class-name frame) (car frame))
(define (class-parent frame) (cadr frame))
(define (class-instance-fields frame) (caddr frame))
(define (class-static-fields frame) (cadddr frame))
(define (class-instance-functions frame) (cadddr (cdr frame)))
(define (calss-static-functions frame) (cadddr (cddr frame)))
(define (class-constructors frame) (cadddr (cdddr frame)))


; Changes a variable binding by placing the new value in the appropriate place in the values
; returns the new updated bindings
(define (replace-varval-pair var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (list varlist (cons (scheme->language val) (cdr vallist))))
      (else (add-pair (car varlist) (car vallist) (replace-varval-pair var val (cdr varlist) (cdr vallist))))))
