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
(define top-frame car)
(define remaining-frames cdr)


; function: (name (parameters) (body) (closure))
(define (function-name closure) (car closure))
(define (function-parameters closure)(cadr closure))
(define (function-body closure)(caddr closure))
(define (function-bindings closure)(cadddr closure))

(define (function-formal-name f) (car f))
(define (function-formal-params f) (cadr f))
(define (function-formal-body f) (caddr f))

(define (variables bindings) (car bindings))
(define (vals bindings) (cadr bindings))

; class: (classname parentclass (instancefields) (functions) (constructors))
(define (class-name frame) (car frame))
(define (class-parent frame) (cadr frame))
(define (class-instancefields frame) (caddr frame))
(define (class-functions frame) (cadddr frame))
(define (class-constructors frame) (cadddr (cdr frame)))
