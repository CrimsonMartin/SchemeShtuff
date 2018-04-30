
;checks that l2 is a viable input to the function with formal params l1
(define (is-compatible-param-list l1 l2 state)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f); if we get here, then one is not null so the length is mismatched
      ;this is here so it returns and doesn't crash
    ((not (is-compatible? (car l1) (car l2) state)) #f)
    (else (is-compatible-param-list (cdr l1) (cdr l2) state))))

(define (is-compatible? x y state)
    ;since we don't have types, we can't check if it is compatible or not until run time
    #t)


; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define (isequal val1 val2)
  (if (and (number? val1) (number? val2))
    (= val1 val2)
    (eq? val1 val2)))


(define (language->scheme v)
(cond
  ((eq? v 'false) #f)
  ((eq? v 'true) #t)
  (else v)))

(define (scheme->language v)
(cond
  ((eq? v #f) 'false)
  ((eq? v #t) 'true)
  (else v)))

; Get the location of a name in a list of names
(define (indexof var l)
  (cond
    ((null? l) 0)  ; should not happen
    ((eq? var (car l)) 0)
    (else (+ 1 (indexof var (cdr l))))))

  ; Get the value stored at a given index in the list
(define (get-value n l)
  (cond
    ((zero? n) (car l))
    (else (get-value (- n 1) (cdr l)))))

; does a variable exist in a list?
(define (exists-in-list? var l)
  (cond
    ((null? l) #f)
    ((eq? var (car l)) #t)
    (else (exists-in-list? var (cdr l)))))


(define call/cc call-with-current-continuation)

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define (myerror str . vals)
(letrec ((makestr (lambda (str vals)
  (if (null? vals) str
  (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
  (error-break (display (string-append str (makestr "" vals))))))
