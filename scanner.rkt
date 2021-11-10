#lang racket

(define (number? potential-number)
  (cond
    [(equal? #f potential-number) #f]
    [else #t]))

(define (keyword? potential-keyword)
  (cond
    [(equal? (list "read") potential-keyword) #t]
    [(equal? (list "write") potential-keyword) #t]
    [else #f]))

(define (id? potential-id)
  (cond
    [(equal? #f potential-id) #f]
    [else #t]))

(define (get-tokens input-string [rest-string input-string] [char-index 0])
  (define token (string-trim (first (string-split rest-string))  " " #:repeat? #t))
  (define potential-number (regexp-match #rx"[0-9]+\\.?[0-9]*|\\.[0-9]+" token))
  (define potential-id (regexp-match #rx"[a-zA-Z][a-zA-Z0-9]*" token))

  ;(print potential-id)
  ;(printf "\r\n")
  ;(print potential-number)
  ;(printf "\r\n\r\n")
  
  (cond
    ; checks to see if we're at the end of input and will only execute if there's no $$ 
    [(equal? (string-length input-string) char-index) (list)]

    ; checks for end program "$$"
    [(equal? (substring input-string char-index (+ char-index 2)) "$$") '("$$")]
    
    ; checks for a space " "
    [(equal? (substring input-string char-index (+ char-index 1)) " ") (get-tokens input-string (substring input-string (+ char-index 1)) (+ char-index 1))]

    ; checks for a newline character "\r\n"
    [(equal? (substring input-string char-index (+ char-index 2)) "\r\n") (append '("\n") (get-tokens input-string (substring input-string (+ char-index 2)) (+ char-index 2)))]

    ; checks for a tab character "\t"
    [(equal? (substring input-string char-index (+ char-index 2)) "\t") (get-tokens input-string (substring input-string (+ char-index 2)) (+ char-index 2))]
    
    ; checks for left parenthesis "("
    [(equal? (substring input-string char-index (+ char-index 1)) "(") (append '("(") (get-tokens input-string (substring input-string (+ char-index 1)) (+ char-index 1)))]

    ; checks for right parenthesis ")"
    [(equal? (substring input-string char-index (+ char-index 1)) ")") (append '(")") (get-tokens input-string (substring input-string (+ char-index 1)) (+ char-index 1)))]

    ; checks for + operator
    [(equal? (substring input-string char-index (+ char-index 1)) "+") (append '("+") (get-tokens input-string (substring input-string (+ char-index 1)) (+ char-index 1)))]

    ; checks for - operator
    [(equal? (substring input-string char-index (+ char-index 1)) "-") (append '("-") (get-tokens input-string (substring input-string (+ char-index 1)) (+ char-index 1)))]

    ; checks for * operator
    [(equal? (substring input-string char-index (+ char-index 1)) "*") (append '("*") (get-tokens input-string (substring input-string (+ char-index 1)) (+ char-index 1)))]

    ; checks for / operator
    [(equal? (substring input-string char-index (+ char-index 1)) "/") (append '("/") (get-tokens input-string (substring input-string (+ char-index 1)) (+ char-index 1)))]

    ; checks for := operator
    [(equal? (substring input-string char-index (+ char-index 2)) ":=") (append '(":=") (get-tokens input-string (substring input-string (+ char-index 1)) (+ char-index 2)))]

    ; check for unidentified character before matching with identifiers
    ;[(not-equal? (regexp-match #rx"^[a-zA-Z0-9]" input-string) (substring input-string char-index (+ char-index 1)))]

    ; checks for a number
    [(number? potential-number) (append (list "number") (get-tokens input-string (substring input-string (+ char-index (string-length (first potential-number)))) (+ char-index (string-length (first potential-number)))))]
    
    ; checks for a keyword
    [(keyword? potential-id) (append potential-id (get-tokens input-string (substring input-string (+ char-index (string-length (first potential-id)))) (+ char-index (string-length (first potential-id)))))]
    
    ; checks for an id
    [(id? potential-id) (append (list (string-append "id(" (first potential-id) ")")) (get-tokens input-string (substring input-string (+ char-index (string-length (first potential-id)))) (+ char-index (string-length (first potential-id)))))]
  
    ; throws scanner error if nothing above evaluated
    [else (list "error" token)])
    )
  

(define (scan-file fileName)
  (define input-port (open-input-file fileName))
  (define input-string (port->string input-port))
  (get-tokens input-string))
  ;(print input-string))

(provide scan-file)