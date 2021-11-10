#lang racket
(require (file "scanner.rkt"))


(define (parse inputFile)
  (define token-list (scan-file inputFile))
  (define parse-result (program token-list))
  (cond
    [(equal? (second (reverse token-list)) "error") (string-append (string-append "Syntax Error on line " (~v (+ (count newline-char? token-list) 1)) ". Unexpected token") ": '" (first (reverse token-list)) "'")]
    [else
     (cond
       [(equal? (first parse-result) "$$") "Program accepted, successfully parsed."]
       [else (string-append "Syntax error on line " (~v (+ (count newline? (third parse-result)) 1)) ". Unexpected token: '" (second parse-result) "'")])
     ]))


(define (program tokens) ; expected tokens: {id, read, write, $$}
  (cond
    [(equal? (first tokens) "\n") (program (append (rest tokens) '("newline")))]
    [(id? tokens) (stmt_list tokens)]
    [(equal? (first tokens) "read") (stmt_list tokens)]
    [(equal? (first tokens) "write") (stmt_list tokens)]
    [(equal? (first tokens) "$$") '("$$")]
    [else (list #f (first tokens) (rest tokens))] ; throw error
  ))


(define (stmt_list tokens) ; expected tokens: {id, read, write} | {$$}
  (cond
    [(equal? (first tokens) #f) tokens] ; bubble error back to top
    [(equal? (first tokens) "\n") (stmt_list (append (rest tokens) '("newline")))]
    [(id? tokens) (stmt_list (stmt tokens))]
    [(equal? (first tokens) "read") (stmt_list (stmt tokens))]
    [(equal? (first tokens) "write") (stmt_list (stmt tokens))]
    [(equal? (first tokens) "$$") tokens]
    [else (list #f (first tokens) (rest tokens))] ; throw error
  ))


(define (stmt tokens) ; expected tokens: {id} | {read} | {write}
  (cond
    [(equal? (first tokens) #f) tokens] ; bubble error back to top
    [(equal? (first tokens) "\n") (stmt (append (rest tokens) '("newline")))]
    [(id? tokens) (expr (assignment-op? (rest tokens)))]
    [(equal? (first tokens) "read") (cond
                                      [(id? (rest tokens)) (rest (rest tokens))]
                                      [else (list #f (first tokens) (rest tokens))])] ; throw error
    [(equal? (first tokens) "write") (expr (rest tokens))]
    [else (list #f (first tokens) (rest tokens))] ; throw error
  )) 


(define (expr tokens) ; expected tokens: {(, id, number}
  (term_tail (term tokens)))


(define (term_tail tokens) ; expected tokens: {+, -} | {), id, read, write, $$}
  (cond
    [(equal? (first tokens) #f) tokens] ; bubble error back to top
    [(equal? (first tokens) "\n") (term_tail (append (rest tokens) '("newline")))]
    [(equal? (list (first tokens)) (regexp-match #rx"^\\+|\\-" (first tokens))) (term_tail (term (add_op tokens)))]
    [(equal? (list (first tokens)) (regexp-match #rx"^\\)" (first tokens))) tokens ]
    [(id? tokens) tokens ]
    [(equal? (first tokens) "read") tokens]
    [(equal? (first tokens) "write") tokens]
    [(equal? (first tokens) "$$") tokens]
    [else (list #f (first tokens) (rest tokens))] ; throw error
  )
  )


(define (term tokens) ; expected tokens: {(, id, number}
  (factor_tail (factor tokens)))


(define (factor_tail tokens)  ; expected tokens: {*, /} | {+, -, ), id, read, write $$}
  (cond
    [(equal? (first tokens) #f) tokens] ; bubble error back to top
    [(equal? (first tokens) "\n") (factor_tail (append (rest tokens) '("newline")))]
    [(equal? (list (first tokens)) (regexp-match #rx"^\\*|\\/" (first tokens))) (factor_tail (factor (mult_op tokens)))]
    [(equal? (list (first tokens)) (regexp-match #rx"^\\+|\\-|\\)" (first tokens))) tokens]
    [(id? tokens) tokens]
    [(equal? (first tokens) "read") tokens]
    [(equal? (first tokens) "write") tokens]
    [(equal? (first tokens) "$$") tokens]
    [else (list #f (first tokens) (rest tokens))] ; throw error
  ))


(define (factor tokens) ; expected tokens: {(} | {id} | {number}
  (cond
    [(equal? (first tokens) #f) tokens] ; bubble error back to top
    [(equal? (first tokens) "\n") (factor (append (rest tokens) '("newline")))]
    [(equal? (first tokens) "(") (rparen? (expr (rest tokens))) ] 
    [(id? tokens) (rest tokens)]
    [(equal? (first tokens) "number") (rest tokens)]
    [else (list #f (first tokens) (rest tokens))] ; throw error
  ))


(define (add_op tokens) ; expected tokens: {+} | {-}
  (cond
    [(equal? (first tokens) "+") (rest tokens)]
    [(equal? (first tokens) "-") (rest tokens)]
   ))


(define (mult_op tokens) ; expected tokens: {*} | {/}
  (cond
    [(equal? (first tokens) "*") (rest tokens)]
    [(equal? (first tokens) "/") (rest tokens)]
  ))


(define (id? tokens)
  (cond
    [(equal? (list (first tokens))  (regexp-match #rx"^id\\([a-zA-Z][a-zA-Z0-9]*\\)$" (first tokens))) #t]
    [else #f]
    )
  )


(define (assignment-op? tokens)
  (cond
    [(equal? (first tokens) ":=") (rest tokens)]
    [else (list #f (first tokens) (rest tokens))] ; throw error
  ))


(define (rparen? tokens)
  (cond
    [(equal? (first tokens) ")") (rest tokens)]
    [else (list #f (first tokens) (rest tokens))] ; throw error
    )
  )


(define (newline-char? token)
  (cond
    [(equal? token "\n") #t]
    [else #f]))


(define (newline? token)
  (cond
    [(equal? token "newline") #t]
    [else #f]))