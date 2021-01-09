#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define simple-math-lexer
           (lexer
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme)))
            ("+" (token-plus))
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens value-tokens (NUM))
(define-empty-tokens empty-tokens (EOF plus))

(define basic-parser
           (parser
             (start command)
             (end EOF)
             (error void)
             (tokens value-tokens empty-tokens)
             (grammar

               (command
                  [(keyword)                $1]
                  [(command SEMIC keyword) (list $1 $3)]
                )

               (keyword
                  [(if_statement)         $1]
                  [(assignment_statement) $1]
                  [(while_statement)      $1]
                  [(return)               $1]
                )

               (while_statement
                  [(WHILE exp DO command END) (list 'while $2 $4)]
                )

               (if_statement
                  [(IF exp THEN command ELSE command END) (list 'if $2 $4 $6)]
                )

               (assignment_statement
                  [(VARIABLE ASSIGN exp) (list 'assign $1 $3)]
                )

               (return
                  [(RETURN exp) (list 'return $2)]
                )

               (exp
                  [(aexp) $1]
                  [(aexp GT aexp)  (list 'gt $1 $3)]
                  [(aexp LT aexp)  (list 'lt $1 $3)]
                  [(aexp BEQ aexp) (list 'beq $1 $3)]
                  [(aexp BNE aexp) (list 'bne $1 $3)]
                )

               (aexp
                  [(bexp)             $1]
                  [(bexp MINUS aexp) (list 'sub $1 $3)]
                  [(bexp PLUS aexp)  (list 'add $1 $3)]
                )

               (bexp
                  [(cexp)            $1]
                  [(cexp MULT bexp) (list 'mul $1 $3)]
                  [(cexp DIV bexp)  (list 'div $1 $3)]
                )

               (cexp
                  [(MINUS cexp)          (list 'minus $2)]
                  [(LP exp RP)           (list 'parenthese $2)]
                  [(NUMBER)              (list 'number $1)]
                  [(NULL)                (list 'null)]
                  [(VARIABLE)            (list 'variable $1)]
                  [(TRUE)                (list 'true)]
                  [(FALSE)               (list 'false)]
                  [(STRING)              (list 'string $1)]
                  [(list)                (list 'list $1)]
                  [(VARIABLE listMember) (list 'access $1 $2)]
                )

               (list
                  [(LB listValues RB) $2]
                  [(LB RB)            'emptyList]
                )

               (listValues
                  [(exp)                  (list 'aslist $1)]
                  [(exp COMMA listValues) (list 'append $1 $3)]
                )

               (listMember
                  [(LB exp RB)             $2]
                  [(LB exp RB listMember) (list $2 $4)]
                )
              )
            )
 )

(define initial-env '())
(define (set-variable name value env) (cons (cons name value) env))
(define (handle-eval valenv) (eval (first valenv) (second valenv)))
(define (get-variable name env) (handle-eval (cond [(assoc name env) => cdr] [else (raise "error")])))
(define (set-vars vars args def-env cur-env)
    (cond
      [(empty? vars) def-env]
      [else (set-vars (rest vars) (rest args) (set-variable (first vars) (list (first args) cur-env) def-env) cur-env)]
     )
 )

;GT '>'
(define (eval-gt args env)
  (let
     (
       [o1 (eval (first args) env)]
       [o2 (eval (second args) env)]
      )
      (cond
        [(and (number? o1) (number? o2)) (> o1 o2)]
        [(and (string? o1) (string? o2)) (string>? o1 o2)]
        [(and (list? o1) (number? o2)) (reduce and-f (broadcast > o1 o2))]
        [(and (number? o1) (list? o2)) (reduce and-f (broadcast < o2 o1))]
        [(and (list? o1) (string? o2)) (reduce and-f (broadcast string>? o1 o2))]
        [(and (string? o1) (list? o2)) (reduce and-f (broadcast string<? o2 o1))]
       )
   )
 )
;LT '<'
(define (eval-lt args env)
  (let
      (
        [o1 (eval (first args) env)]
        [o2 (eval (second args) env)]
       )
      (cond
        [(and (number? o1) (number? o2)) (< o1 o2)]
        [(and (string? o1) (string? o2)) (string<? o1 o2)]
        [(and (list? o1) (number? o2)) (reduce and-f (broadcast < o1 o2))]
        [(and (number? o1) (list? o2)) (reduce and-f (broadcast > o2 o1))]
        [(and (list? o1) (string? o2)) (reduce and-f (broadcast string<? o1 o2))]
        [(and (string? o1) (list? o2)) (reduce and-f (broadcast string>? o2 o1))]
       )
   )
 )
;BEQ '=='
(define (eval-beq args env)
  (let
      (
        [o1 (eval (first args) env)]
        [o2 (eval (second args) env)]
       )
      (cond
        [(and (number? o1) (number? o2)) (= o1 o2)]
        [(and (string? o1) (string? o2)) (string=? o1 o2)]
        [(and (eq? o1 'n) (eq? o2 'n)) #t]
        [(and (boolean? o1) (boolean? o2)) (boolean=? o1 o2)]
        [(and (list? o1) (list? o2)) (reduce and-f (list-list-op equal? o1 o2 #f))]
;        [(and (list? o1) (number? o2)) (reduce and-f (broadcast = o1 o2))]
;        [(and (number? o1) (list? o2)) (reduce and-f (broadcast = o2 o1))]
;        [(and (list? o1) (string? o2)) (reduce and-f (broadcast string=? o1 o2))]
;        [(and (string? o1) (list? o2)) (reduce and-f (broadcast string=? o2 o1))]
;        [(and (list? o1) (boolean? o2)) (reduce and-f (broadcast boolean=? o1 o2))]
;        [(and (boolean? o1) (list? o2)) (reduce and-f (broadcast boolean=? o2 o1))]
;        [(and (list? o1) (eq? o2 'n)) (reduce and-f (broadcast eq? o1 o2))]
;        [(and (eq? o1 'n) (list? o2)) (reduce and-f (broadcast eq? o2 o1))]
        [else #f]
       )
   )
 )
;BNE '!="
(define (eval-bne args env) (if (eval-beq args env) #f #t))




;test
(define (main input-path)
    (define lex-this (lambda (lexer input) (lambda () (lexer input))))
    (define my-lexer (lex-this pl-lexer (open-input-string (string-join (map ~a (append (file->lines "builtin.txt") (file->lines input-path))) " "))))
    (define parse-tree (extract-commands (let ((parser-res (pl-parser my-lexer))) parser-res)))

    (display (run parse-tree initial-env))
)