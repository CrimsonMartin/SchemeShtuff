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

(define reassignError "error - reassigning")
(define declaringError "error - using a variable before declaring")
(define assigningError "error - using a variable before assigning")
(define undefinedError "Undefined Operator")
(define emptyInputError "error - empty input")
(define invalidStateError "error - invalid state")

;----------------------------------------------------------------------------
; Abstraction for predefined definitions
;----------------------------------------------------------------------------
;Abstraction for initial/empty state
(define initialState '((()())))

;Abstraction for empty layer
(define emptyLayer '(()()))


;----------------------------------------------------------------------------
; Abstraction for miscellaneous helper functions
;----------------------------------------------------------------------------
;Put input into an empty
(define addToEmptyList
  (lambda (n)
    (cons n '())))

;----------------------------------------------------------------------------
; Abstraction for input-related helper functions
;----------------------------------------------------------------------------
;Abstraction for getting first element of input list
(define firstElement
  (lambda (input)
    (car input)))

;Abstraction for removing 1st element of input list
(define restOf
  (lambda (input)
    (cdr input)))

;Abstraction for getting second element of input list
(define secondElement
  (lambda (input)
    (cadr input)))

;Abstraction for getting first 2 elements of input list
(define restOf2
  (lambda (input)
    (cddr input)))

;Abstraction for getting third element of input list
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
    (cons (restOf (getLayerVar layer)) (addToEmptyList (restOf (getLayerVal layer))))))

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
                (addToEmptyList (cons 'UNDEF (getLayerVal (getLayer state)))))
          (removeLayer state))))

;Set a variable's assigned value to val
(define setVar-cps
  (lambda (var val state return)
    (cond
      ((null? state) (return (error declaringError)))
      ((or (null? (getLayerVar (getLayer state))) (null? (getLayer state)))
       (setVar-cps var val (restOf state) (lambda (newState) (return (cons (firstElement state) newState)))))
      ((eq? var (firstElement (getLayerVar (getLayer state))))
       (cons (cons (getLayerVar (getLayer state)) (addToEmptyList (cons val (restOf (getLayerVal (getLayer state)))))) (restOf state)))
      (else
       (setVar-cps var val (cons (removeFirstVarPair (getLayer state)) (restOf state))
                   (lambda (newState) (return (cons (cons (cons (firstElement (getLayerVar (getLayer state))) (getLayerVar (getLayer newState)))
                                                    (addToEmptyList (cons (firstElement (getLayerVal (getLayer state))) (getLayerVal (getLayer newState))))) (restOf state)))))))))

(define setVar
  (lambda (var val state)
    (setVar-cps var val state (lambda (v) v))))

;Test template: (setVar 'a '2 '(((a b c) (1 2 3)) ((d e f) (4 5 6))))


;----------------------------------------------------------------------------
; Abstraction for variable-values-related helper functions
;----------------------------------------------------------------------------
;Abstraction for getting value list from layer
(define getLayerVal
  (lambda (layer)
    (secondElement layer)))

;Return the value of the first occurence of a variable (x) in state
;Throws error if variable doesn't exist, or if value does not exist
(define getXVal-helper
  (lambda (x state)
    (cond
      ((null? state) (error declaringError))
      ((or (null? (getLayerVar (getLayer state))) (null? (getLayer state))) (getXVal x (restOf state)))
      ;((null? (getLayerVal (getLayer state))) (error assigningError)); Unnecessary
      ((eq? x (firstElement (getLayerVar (getLayer state)))) (firstElement (getLayerVal (getLayer state))))
      (else (getXVal x (cons (removeFirstVarPair (getLayer state)) (removeLayer state)))))))

(define getXVal
  (lambda (x state)
    (if (eq? 'UNDEF (getXVal-helper x state))
        (error assigningError)
        (getXVal-helper x state))))

;------------------------------------------------------------------------------------------
;interpreter methods
;------------------------------------------------------------------------------------------
;Main function
(define interpret
  (lambda (filePathName)
    (read (parser filePathName) initialState)))

;Read
;Denotes expression and assign expressions to further functions
(define read-cps
  (lambda (expr state return)
    (cond
      ((null? expr) return (error emptyInputError))
      ((null? state) return (error invalidStateError))
      ((list? firstElement expr) (read-cps (car expr) state (lambda (newState) (return (read-cps newState return)))))
      ((isMember? (firstElement expr) '(< > <= >= == != && ||)) (booleanEvaluate expr state return))
      ((isMember? (firstElement expr) '(+ - * / %))  (intEvaluate expr state return))
      ((isMember? (firstElement expr)
                '(begin try catch finally continue var = if while return))
       (stateEvaluate expr state return))
      (else (error undefinedError)))))

(define read
  (lambda (expr state)
    (read-cps (expr state (lambda (v) v)))))

;helper for the read method, to determine which helper method to call
(define (isMember? a lis)
  (cond
    ((null? lis) #f)
    ((equal? a (car lis)) #t)
    (else (isMember? a (cdr lis)))))

;------------------------------------------------------------------------------------------
;M_state methods
;------------------------------------------------------------------------------------------
(define stateEvaluate-helper
  (lambda (expr state break)
    (cond
      ;Null check
      ((null? expr) (error emptyInputError))
      ;Declare var
      ((eq? (car expr) 'var) (cond
                                    ((null? (restOf2 expr)) (addVar (secondElement expr) state))
                                    ;(else (setStateVar (secondElement expr) (M_value (3rdInput expr) (declareVar (2ndInput statement) state)) (declareVar (2ndInput statement) state)))))
      )))
    ))

(define stateEvaluate
  (lambda (expr state)
    (call/cc
     (lambda (break)
       (stateEvaluate-helper expr state break)))))

;------------------------------------------------------------------------------------------
;M_value methods
;------------------------------------------------------------------------------------------
(define intEvaluate
  (lambda (expr state)
    (cond
      ;Null check
      ((null? expr) (error 'M_value "Evaluating an empty expression"))
      ;Number check
      ((number? expr) expr)
      ;List check No need to check for operators if input is not a list
      ((pair? expr)
          (cond
            ;Uniary -
            ((and (eq? (firstElement expr) '-) (null? (restOf2 expr))) (* -1 (intEvaluate (thirdElement expr) state)))
            ;+
            ((eq? '+ (firstElement expr)) (+ (intEvaluate (secondElement expr) state) (intEvaluate (thirdElement expr) state)))
            ;-
            ((eq? '- (firstElement expr)) (- (intEvaluate (secondElement expr) state) (intEvaluate (thirdElement expr) state)))
            ;*
            ((eq? '* (firstElement expr)) (* (intEvaluate (secondElement expr) state) (intEvaluate (thirdElement expr) state)))
            ;/
            ((eq? '/ (firstElement expr)) (quotient (intEvaluate (secondElement expr) state) (intEvaluate (thirdElement expr) state)))
            ;%
            ((eq? '% (firstElement expr)) (remainder (intEvaluate (secondElement expr) state) (intEvaluate (thirdElement expr) state)))
            ;Unknown Operator
            (else (error undefinedError))))
      ;See a variable
      (else (getXVal expr state)))))

;Test template: (intEvaluate '(+ 1 2) '(((a b c) (1 2 3)) ((d e f) (4 5 6))))

;------------------------------------------------------------------------------------------
;M_boolean methods
;------------------------------------------------------------------------------------------
;if (booleanevaluate) 'true' else 'false'
(define booleanEvaluate-helper
  (lambda (expr state)
    (cond
      ;Null check
      ((null? expr) (error emptyInputError))
      ((null? state) (error invalidStateError))
      ;check raw boolean
      ((equal? expr 'true) #t)
      ((eq? expr 'false) #f)
      ;==
      ((eq? (firstElement expr) '==) (eq? (intEvaluate (secondElement expr) state) (intEvaluate (thirdElement expr) state)))
      ;!=
      ((eq? (firstElement expr) '!=) (not (intEvaluate (secondElement expr) state) (intEvaluate (thirdElement expr) state)))
      ;<
      ((eq? (firstElement expr) '<) (< (intEvaluate (secondElement expr) state) (intEvaluate (thirdElement expr) state)))
      ;>
      ((eq? (firstElement expr) '>) (> (intEvaluate (secondElement expr) state) (intEvaluate (thirdElement expr) state)))
      ;<=
      ((eq? (firstElement expr) '<=) (<= (intEvaluate (secondElement expr) state) (intEvaluate (thirdElement expr) state)))
      ;>=
      ((eq? (firstElement expr) '>=) (>= (intEvaluate (secondElement expr) state) (intEvaluate (thirdElement expr) state)))
      ;||
      ((eq? (firstElement expr) '||) (or (booleanEvaluate (secondElement expr) state)  (booleanEvaluate (thirdElement expr) state)))
      ;&&
      ((eq? (firstElement expr) '&&) (and (booleanEvaluate (secondElement expr) state)  (booleanEvaluate (thirdElement expr) state)))
      ;Unrecognized operator
      (else (error 'undefinedError)))))

;Turn #t/#f to 'true/'false
(define booleanEvaluate
  (lambda (expression state)
    (if (booleanEvaluate)
        'true
        'false)))

;------------------------------------------------------------------------------------------
;Flow control methods
;------------------------------------------------------------------------------------------

(define (m_for statement1 condition statement2 statement3 cstate break)
  (if (booleanEvaluate condition (stateEvaluate statement1 cstate) break)
      (m_for '() condition statement2 statement3 (stateEvaluate statement2 (stateEvaluate statement3 (stateEvaluate statement1 cstate))) break)
      (stateEvaluate (statement1 cstate) break)))

(define (m_if condition then else cstate)
  (if (booleanEvaluate condition) (stateEvaluate then cstate)
      (stateEvaluate else cstate)))

;if condition is true then run statement and repeat
(define (m_while condition statement cstate break return)
  (if (booleanEvaluate condition) (m_while condition (stateEvaluate statement cstate) break)
      cstate))

;TODO add break return and continue
