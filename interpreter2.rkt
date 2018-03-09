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
; Miscellaneous abstractions
;----------------------------------------------------------------------------
;Put input into an empty
(define addToEmptyList
  (lambda (n)
    (cons n '())))

;Undefined
(define undefined 'UNDEF)

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

;Abstraction for getting fourth element of input list
(define fourthElement
  (lambda (input)
    (cadddr input)))
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

;Add a variable and its value to first layer of state
(define addVar
  (lambda (var val state)
    (cons (cons (cons var (getLayerVar(getLayer state)))
                (addToEmptyList (cons val (getLayerVal (getLayer state)))))
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
    (if (eq? undefined (getXVal-helper x state))
        (error assigningError)
        (getXVal-helper x state))))

;------------------------------------------------------------------------------------------
;interpreter methods
;------------------------------------------------------------------------------------------
;Main function
(define interpret-helper
  (lambda (filePathName mainBreak)
    (read (parser filePathName) initialState mainBreak)))

(define interpret
  (lambda (filePathName)
    (call/cc
     (lambda (mainBreak)
       (interpret-helper filePathName mainBreak)))))

;Read
;Denotes expression and assign expressions to further functions
(define read-cps
  (lambda (expr state return mainBreak subExprBreak)
    (cond
      ((null? expr) return (subExprBreak (removeLayer state)))
      ((null? state) return (error invalidStateError))
      ((list? firstElement expr) (read-cps (car expr) state (lambda (newState) (return (read-cps newState return))) mainBreak))
      ((isMember? (firstElement expr) '(< > <= >= == != && ||)) (return (booleanEvaluate expr state)))
      ((isMember? (firstElement expr) '(+ - * / %))  (return (intEvaluate expr state)))
      ((isMember? (firstElement expr)
                '(begin try catch throw finally break continue var = if while return))
       (return (stateEvaluate expr state mainBreak subExprBreak)))
      (else (error undefinedError)))))

(define read
  (lambda (expr state mainBreak)
    (call/cc
     (lambda (subExpressionBreak
              (read-cps (expr state (lambda (v) v) mainBreak subExpressionBreak)))))))

;helper for the read method, to determine which helper method to call
(define (isMember? a lis)
  (cond
    ((null? lis) #f)
    ((equal? a (car lis)) #t)
    (else (isMember? a (cdr lis)))))

;------------------------------------------------------------------------------------------
;M_state methods
;------------------------------------------------------------------------------------------
(define stateEvaluate
  (lambda (expr state mainBreak)
    (cond
      ;Null check
      ((null? expr) (error emptyInputError))
      ;Declare var
      ((eq? (firstElement expr) 'var) (stateVar expr state))
      ;set the value of a var
      ((eq? (firstElement expr) '=) (stateEqual expr state))
      ;Return something
      ((eq? (firstElement expr) 'return) (mainBreak (stateReturn expr state)))
      ;Brackets
      ((eq? (firstElement expr) 'begin) stateBracket)
      
      ((eq? (firstElement expr) 'try)   )
      ((eq? (firstElement expr) 'catch)   )
      ((eq? (firstElement expr) 'finally)   )
      ((eq? (firstElement expr) 'continue)   )
      ;flow control
      ((eq? (firstElement expr) 'if) (m_if (secondElement expr) thenexpr elseexpr state break))
      ((eq? (firstElement expr) 'while) (m_while (secondElement expr) body state break))
      ((eq? (firstElement expr) 'for) (m_for statement1 condition statement2 statement3 state break))
      ;break continuation
      ((eq? (firstElement expr) 'return) (break (evaluate (restOf expr)) ))

    )))

;Abstraction for handling var
(define stateVar
  (lambda (expr state)
    (cond
      ((null? (restOf2 expr)) (addVar (secondElement expr) undefined state))
      (else (addVar (secondElement expr) (thirdElement expr) state)))))

;Abstraction for handling =
(define stateEqual
  (lambda (expr state)
    (cond
      ((null? expr) (error emptyInputError))
      ((null? state) (error invalidStateError))
      (else (setVar (secondElement expr) (intEvaluate (thirdElement state) state) state)))))

;Abstraction for handling return
(define stateReturn
  (lambda (expr state)
    (intEvaluate (secondElement expr) state)))

;Abstraction for handling begin (brackets)
(define stateBegin
  (lambda (expr state mainBreak)
    (read (secondElement expr) (addLayer state) mainBreak)))

(define stateWhile
  (lambda (expr state break)
    (if booleanEvaluate(

(define (m_if condition then else cstate)
  (if (booleanEvaluate condition) (stateEvaluate then cstate)
      (stateEvaluate else cstate)))

;if condition is true then run statement and repeat
(define (m_while condition statement cstate break return)
  (if (booleanEvaluate condition) (m_while condition (stateEvaluate statement cstate) break)

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
            ((and (eq? (firstElement expr) '-) (null? (restOf2 expr))) (* -1 (intEvaluate (secondElement expr) state)))
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
      ;Unrecognized operator
      (else (error 'undefinedError)))))

;Turn #t/#f to 'true/'false
(define booleanEvaluate
  (lambda (expression state)
    (if (booleanEvaluate)
        'true
        'false)))
