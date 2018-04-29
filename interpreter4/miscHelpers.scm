

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
