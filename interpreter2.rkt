;Michael Folz
;Mark Gross
;Kwan Lam Lau kxl417


;main function is called by (interpret "<string of file path and name>")
;the string is in double quotes


#lang racket
(require "simpleParser.scm")

;State is in ((layer1) (layer2)...)
;Layer is in ((var) (val))

;----------------------------------------------------------------------------
; Abstraction for errors
;----------------------------------------------------------------------------

(define reassignError "error- reassigning")
(define declaringError "error- using a variable before declaring")
(define assigningError "error- using a variable before assigning")
(define undefinedError "Undefined Operator")

;----------------------------------------------------------------------------
; Abstraction for predefined definitions
;----------------------------------------------------------------------------
;Abstraction for initial/empty state
(define initialState '((()())))

;Abstraction for empty layer
(define emptyLayer '(()()))

;----------------------------------------------------------------------------
; Abstraction for input-related helper functions
;----------------------------------------------------------------------------
;Abstraction for getting first element of input
(define firstElement
  (lambda (input)
    (car input)))

;Abstraction for getting >1st element of input
(define restOf
  (lambda (input)
    (cdr input)))

;Abstraction for getting second element of input
(define secondElement
  (lambda (input)
    (cadr input)))

;Abstraction for getting third element of input
(define thirdElement
  (lambda (input)
    (caddr input)))

;----------------------------------------------------------------------------
; Abstraction for state manipulation helper functions
;----------------------------------------------------------------------------
;Abstraction for adding layer
(define addLayer
  (lambda (layer state)
    (cons layer state)))

;Abstraction for adding new layer
(define addNewLayer
  (lambda (state)
    (cons emptyLayer state)))

;Abstraction for removing a layer
(define removeLayer
  (lambda (state)
    (restOf state)))

;----------------------------------------------------------------------------
; Abstraction for layer manipulation helper functions
;----------------------------------------------------------------------------
;Abstraction for getting the top layer of state
(define getLayer
  (lambda (state)
    (firstElement state)))

;Abstraction for removing first variable and value from a layer
(define removeFirstVarPair
  (lambda (layer)
    (cons (restOf (getLayerVar layer)) (cons (restOf (getLayerVal layer)) '()))))

;----------------------------------------------------------------------------
; Abstraction for variables-related helper functions
;----------------------------------------------------------------------------
;Abstraction for getting variable list from layer
(define getLayerVar
  (lambda (layer)
    (firstElement layer)))

;Add a variable to first layer of state
(define addVar
  (lambda (var state)
    (cons (cons (cons var (getLayerVar(getLayer state)))
                (cons (cons 'UNDEF (getLayerVal (getLayer state))) '()))
          (removeLayer state))))

;----------------------------------------------------------------------------
; Abstraction for variable-values-related helper functions
;----------------------------------------------------------------------------
;Abstraction for getting value list from layer
(define getLayerVal
  (lambda (layer)
    (secondElement layer)))

;Return the value of the first occurence of a variable (x) from state
;Throws error if variable doesn't exist, or if value does not exist
(define getXVal
  (lambda (x state)
    (cond
      ((null? state) (error declaringError))
      ((or (null? (getLayerVar (getLayer state))) (null? (getLayer state))) (getXVal x (restOf state)))
      ;((null? (getLayerVal (getLayer state))) (error assigningError)); Unnecessary
      ((eq? x (firstElement (getLayerVar (getLayer state)))) (firstElement (getLayerVal (getLayer state))))
      (else (getXVal x (cons (removeFirstVarPair (getLayer state)) (removeLayer state)))))))

