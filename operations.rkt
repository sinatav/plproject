#lang racket

(require (except-in eopl #%module-begin))

(define (is-all-num list)
  (cond
    [(null? list) #t]
    [(number? (car list)) (is-all-num (cdr list))]
    [else #f]
    ))

(define (is-all-boolean list)
  (cond
    [(null? list) #t]
    [(boolean? (car list)) (is-all-boolean (cdr list))]
    [else #f]
    ))

(define (is-all-str list)
  (cond
    [(null? list) #t]
    [(string? (car list)) (is-all-str (cdr list))]
    [else #f]
    ))

(define (greater-list-num? list num)
  (cond
    [(boolean=? (is-all-num list) #f) (eopl:error "The list elements must be numbers.")]
    [(null? list) #t]
    [(<= (car list) num) #f]
    [else (greater-list-num? (cdr list) num)]
    ))
  

(define (less-list-num? list num)
  (cond
    [(boolean=? (is-all-num list) #f) (eopl:error "The list elements must be numbers.")]
    [(null? list) #t]
    [(>= (car list) num) #f]
    [else (less-list-num? (cdr list) num)]
    ))

(define (greater-num-list? num list)
  (cond
    [(boolean=? (is-all-num list) #f) (eopl:error "The list elements must be numbers.")]
    [(null? list) #t]
    [(<= num (car list)) #f]
    [else (greater-num-list? num (cdr list))]
    ))
  

(define (less-num-list? num list)
  (cond
    [(boolean=? (is-all-num list) #f) (eopl:error "The list elements must be numbers.")]
    [(null? list) #t]
    [(>= num (car list)) #f]
    [else (less-num-list? num (cdr list))]
    ))

(define (greater-list-str? list str)
  (cond
    [(boolean=? (is-all-str list) #f) (eopl:error "The list elements must be strings.")]
    [(null? list) #t]
    [(string<=? (car list) str) #f]
    [else (greater-list-str? (cdr list) str)]
    ))

(define (less-list-str? list str)
  (cond
    [(boolean=? (is-all-str list) #f) (eopl:error "The list elements must be strings.")]
    [(null? list) #t]
    [(string>=? (car list) str) #f]
    [else (less-list-str? (cdr list) str)]
    ))

(define (greater-str-list? str list)
  (cond
    [(boolean=? (is-all-str list) #f) (eopl:error "The list elements must be strings.")]
    [(null? list) #t]
    [(string<=? str (car list)) #f]
    [else (greater-str-list? str (cdr list))]
    ))

(define (less-str-list? str list)
  (cond
    [(boolean=? (is-all-str list) #f) (eopl:error "The list elements must be strings.")]
    [(null? list) #t]
    [(string>=? str (car list)) #f]
    [else (less-str-list? str (cdr list))]
    ))

(define (list-equal? l1 l2)
  (cond
    [(and (null? l1) (null? l2)) #t]
    [(and (null? l1) (not (null? l2))) #f]
    [(and (not (null? l1)) (null? l2)) #f]
    [(and (number? (car l1))(equal? (car l1) (car l2))) (list-equal? (cdr l1) (cdr l2))]
    [(and (string? (car l1))(string=? (car l1) (car l2))) (list-equal? (cdr l1) (cdr l2))]
    [(and (boolean? (car l1))(boolean=? (car l1) (car l2))) (list-equal? (cdr l1) (cdr l2))]
    [(and (list? (car l1))(list-equal? (car l1) (car l2))) (list-equal? (cdr l1) (cdr l2))]
    [else #f]
    ))

(define (boolean-or b1 b2) (or b1 b2))
(define (boolean-and b1 b2) (and b1 b2))

(define (list-op-num L operand num)
  (cond
    [(boolean=? (is-all-num L) #f) (eopl:error "The list elements must be numbers.")]
    [(null? L) null]
    [else (append (list(arithmetic-op (car L) operand num)) (list-op-num (cdr L) operand num))]
    ))

(define (num-op-list L operand num)
  (cond
    [(boolean=? (is-all-num L) #f) (eopl:error "The list elements must be numbers.")]
    [(null? L) null]
    [else (append (list(arithmetic-op num operand (car L))) (num-op-list (cdr L) operand num))]
    ))

(define (list-op-boolean L operand num)
  (cond
    [(boolean=? (is-all-boolean L) #f) (eopl:error "The list elements must be booleans.")]
    [(null? L) null]
    [else (append (list(arithmetic-op (car L) operand num)) (list-op-boolean (cdr L) operand num))]
    ))

(define (str-plus-list L str)
  (cond
    [(boolean=? (is-all-str L) #f) (eopl:error "The list elements must be strings.")]
    [(null? L) null]
    [else (append (list(string-append str (car L))) (str-plus-list (cdr L) str))]
    ))

(define (list-plus-str L str)
  (cond
    [(boolean=? (is-all-str L) #f) (eopl:error "The list elements must be strings.")]
    [(null? L) null]
    [else (append (list(string-append (car L) str)) (list-plus-str (cdr L) str))]
    ))

(define (greater? arg1 arg2)
  (cond
    [(and (number? arg1) (number? arg2) (> arg1 arg2)) #t]
    [(and (number? arg1) (number? arg2) (< arg1 arg2)) #f]
    [(and (number? arg1) (number? arg2) (= arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string>? arg1 arg2)) #t]
    [(and (string? arg1) (string? arg2) (string<? arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string=? arg1 arg2)) #f]
    [(and (list? arg1) (number? arg2)) (greater-list-num? arg1 arg2)]
    [(and (number? arg1) (list? arg2)) (greater-num-list? arg1 arg2)]
    [(and (list? arg1) (string? arg2)) (greater-list-str? arg1 arg2)]
    [(and (string? arg1) (list? arg2)) (greater-str-list? arg1 arg2)]
    [else (eopl:error "two types are not comparable.")]
    ))

(define (less? arg1 arg2)
  (cond
    [(and (number? arg1) (number? arg2) (< arg1 arg2)) #t]
    [(and (number? arg1) (number? arg2) (> arg1 arg2)) #f]
    [(and (number? arg1) (number? arg2) (= arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string<? arg1 arg2)) #t]
    [(and (string? arg1) (string? arg2) (string>? arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string=? arg1 arg2)) #f]
    [(and (list? arg1) (number? arg2)) (less-list-num? arg1 arg2)]
    [(and (number? arg1) (list? arg2)) (less-num-list? arg1 arg2)]
    [(and (list? arg1) (string? arg2)) (less-list-str? arg1 arg2)]
    [(and (string? arg1) (list? arg2)) (less-str-list? arg1 arg2)]
    [else (eopl:error "two types are not comparable.")]
    ))

(define (equality? arg1 arg2)
  (cond
    [(and (number? arg1) (number? arg2) (= arg1 arg2)) #t]
    [(and (number? arg1) (number? arg2) (> arg1 arg2)) #f]
    [(and (number? arg1) (number? arg2) (< arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string=? arg1 arg2)) #t]
    [(and (string? arg1) (string? arg2) (string>? arg1 arg2)) #f]
    [(and (string? arg1) (string? arg2) (string<? arg1 arg2)) #f]
    [(and (null? arg1) (null? arg2)) #t]
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #t) (boolean=? arg2 #t)) #t]
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #t) (boolean=? arg2 #f)) #f]
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #f) (boolean=? arg2 #f)) #t]
    [(and (boolean? arg1) (boolean? arg2) (boolean=? arg1 #f) (boolean=? arg2 #t)) #t]
    [(and (list? arg1) (list? arg2)) (list-equal? arg1 arg2)]
    [else (eopl:error "two types are not comparable.")]
    ))

(define (inequality? arg1 arg2) (not (equality? arg1 arg2)))

(define (negate arg1)
  (cond
    [(number? arg1) (* arg1 -1)]
    [(boolean? arg1) (not arg1)]
    [(list? arg1) (list-op-num arg1 "*" -1)]
    [else (eopl:error "Negation can't be done on the arguement.")]
    ))

(define (arithmetic-op arg1 arg2 arg3)
  (cond
    [(and (number? arg1) (number? arg3) (string=? arg2 "*")) (* arg1 arg3)]
    [(and (number? arg1) (number? arg3) (string=? arg2 "+")) (+ arg1 arg3)]
    [(and (number? arg1) (number? arg3) (string=? arg2 "-")) (- arg1 arg3)]
    [(and (number? arg1) (number? arg3) (string=? arg2 "/") (not(= arg3 0))) (/ arg1 arg3)]
    [(and (number? arg1) (number? arg3) (string=? arg2 "/") (= arg3 0)) (eopl:error "Division By Zero")]
    [(and (number? arg1) (list? arg3)) (num-op-list arg3 arg2 arg1)]
    [(and (list? arg1) (number? arg3)) (list-op-num arg1 arg2 arg3)]
    [(and (boolean? arg1) (boolean? arg3) (string=? arg2 "*")) (boolean-and arg1 arg3)]
    [(and (boolean? arg1) (boolean? arg3) (string=? arg2 "+")) (boolean-or arg1 arg3)]
    [(and (boolean? arg1) (list? arg3)) (list-op-boolean arg3 arg2 arg1)]
    [(and (list? arg1) (boolean? arg3)) (list-op-boolean arg1 arg2 arg3)]
    [(and (string? arg1) (string? arg3) (string=? arg2 "+")) (string-append arg1 arg3)]
    [(and (list? arg1) (list? arg3) (string=? arg2 "+")) (append arg1 arg3)]
    [(and (string? arg1) (list? arg3) (string=? arg2 "+")) (str-plus-list arg3 arg1)]
    [(and (list? arg1) (string? arg3) (string=? arg2 "+")) (list-plus-str arg1 arg3)]
    [else (eopl:error "Operand can't be applied on these arguements.")]
    ))

(provide (all-defined-out))