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

(define (reassignError) "error- reassigning")
(define (declaringError) "error- using a variable before declaring")
(define (assigningError) "error- using a variable before assigning")
(define (undefinedError) "Undefined Operator")

;----------------------------------------------------------------------------
; Abstraction for predefined definitions
;----------------------------------------------------------------------------
;Abstraction for initial/empty state
(define initialState '((()())))

;Abstraction for empty layer
(define emptyLayer '(()()))

;----------------------------------------------------------------------------
; Abstraction for inputs
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
    (cdr state)))

;Abstraction for getting the top layer of state
(define getLayer
  (lambda (state)
    (car state)))

;----------------------------------------------------------------------------
; Abstraction for layer manipulation helper functions
;----------------------------------------------------------------------------
;Abstraction for getting variable list from layer
(define getLayerVar
  (lambda (layer)
    (car layer)))

;Abstraction for getting value list from layer
(define getLayerVal
  (lambda (layer)
    (cadr layer)))




