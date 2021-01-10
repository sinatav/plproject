#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define basic-lexer
           (lexer
            ["+"                            (token-PLUS)]
            ["-"                            (token-MINUS)]
            ["*"                            (token-MULT)]
            ["/"                            (token-DIV)]
            ["="                            (token-ASSIGN)]
            ["=="                           (token-BEQ)]
            ["!="                           (token-BNE)]
            [">"                            (token-GT)]
            ["<"                            (token-LT)]
            ["("                            (token-LP)]
            [")"                            (token-RP)]
            ["["                            (token-LB)]
            ["]"                            (token-RB)]
            ["{"                            (token-LC)]
            ["}"                            (token-RC)]
            [","                            (token-COMMA)]
            [";"                            (token-SEMIC)]
            ["return"                       (token-RETURN)]
            ["if"                           (token-IF)]
            ["then"                         (token-THEN)]
            ["else"                         (token-ELSE)]
            ["end"                          (token-END)]
            ["while"                        (token-WHILE)]
            ["do"                           (token-DO)]
            ["null"                         (token-NULL)]
            ["switch"                       (token-SWITCH)]
            ["case"                         (token-CASE)]
            ["true"                         (token-TRUE)]
            ["false"                        (token-FALSE)]

            [(:+ alphabetic)                                                                              (token-VARIABLE (string->symbol lexeme))]
            [(:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUMBER   (string->number lexeme))]
            [(:: #\" (complement (:: any-string #\" any-string)) #\")                                  (token-STRING   (substring lexeme 1 (- (string-length lexeme) 1)))]
            [whitespace (basic-lexer input-port)]
            [(eof) (token-EOF)]))

(define-tokens value-tokens
  (NUMBER VARIABLE STRING)
 )

(define-empty-tokens empty-tokens
  (PLUS MINUS MULT DIV ASSIGN BEQ BNE GT LT
   LP RP LB RB LC RC COMMA  SEMIC RETURN IF THEN ELSE
   END WHILE DO NULL SWITCH CASE TRUE FALSE
   EOF)
 )


;------------------------------------------

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


(define (and-override a b) (and a b))
(define (or-override a b) (or a b))

(define (reduce reducer boolean-list)
  (cond
    [(not (list? boolean-list)) boolean-list]
    [(empty? boolean-list)
       (cond
         [(equal? reducer or-override) #f]
         [(equal? reducer and-override) #t]
        )
     ]
     [else (reducer (reduce reducer (first boolean-list)) (reduce reducer (rest boolean-list)))]
    )
 )

(define (check-same-type o1 o2)
    (cond
      [(and (number? o1) (number? o2)) #t]
      [(and (boolean? o1) (boolean? o2)) #t]
      [(and (string? o1) (string? o2)) #t]
      [(and (list? o1) (list? o2)) #t]
      [else #f]
     )
 )

(define (broadcast operation list-args scaler-arg)
  (cond
    [(and (not (list? list-args)) (not (equal? operation >)) (not (equal? operation <)) (not (equal? operation string-append))) (if (check-same-type list-args scaler-arg) (operation list-args scaler-arg) #f)]
    [(empty? list-args) (list)]
    [else (append (list (broadcast operation (first list-args) scaler-arg)) (broadcast operation (rest list-args) scaler-arg))]
   )
 )

(define (list-list-operation operation list1 list2 on-diff-len)
  (cond
    [(and (not (list? list1)) (not (list? list2))) (operation list1 list2)]
    [(and (not (list? list1)) (list? list2)) (list on-diff-len)]
    [(and (list? list1) (not(list? list2))) (list on-diff-len)]
    [(not (= (length list1) (length list2))) (list on-diff-len)]
    [(and (empty? list1) (empty? list2)) (list)]
    [else (append (list (list-list-operation operation (first list1) (first list2) on-diff-len)) (list-list-operation operation (rest list1) (rest list2) on-diff-len))]
   )
 )

(define (elementwise operation list-args)
  (cond
    [(not (list? list-args)) (if (and (equal? operation -) (boolean? list-args)) (not list-args) (operation list-args))]
    [(empty? list-args) (list)]
    [else (append (list (elementwise operation (first list-args))) (elementwise operation (rest list-args)))]
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
        [(and (list? o1) (number? o2)) (reduce and-override (broadcast > o1 o2))]
        [(and (number? o1) (list? o2)) (reduce and-override (broadcast < o2 o1))]
        [(and (list? o1) (string? o2)) (reduce and-override (broadcast string>? o1 o2))]
        [(and (string? o1) (list? o2)) (reduce and-override (broadcast string<? o2 o1))]
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
        [(and (list? o1) (number? o2)) (reduce and-override (broadcast < o1 o2))]
        [(and (number? o1) (list? o2)) (reduce and-override (broadcast > o2 o1))]
        [(and (list? o1) (string? o2)) (reduce and-override (broadcast string<? o1 o2))]
        [(and (string? o1) (list? o2)) (reduce and-override (broadcast string>? o2 o1))]
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
        [(and (list? o1) (list? o2)) (reduce and-override (list-list-operation equal? o1 o2 #f))]
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
(define (eval-bne args env) (not (eval-beq args env)))

;add
(define (eval-add args env)
  (let
      (
        [o1 (eval (first args) env)]
        [o2 (eval (second args) env)]
       )
      (cond
        [(and (number? o1) (number? o2)) (+ o1 o2)]
        [(and (list? o1) (number? o2)) (broadcast + o1 o2)]
        [(and (number? o1) (list? o2)) (broadcast + o2 o1)]
        [(and (boolean? o1) (boolean? o2)) (or o1 o2)]
        [(and (list? o1) (boolean? o2)) (broadcast or-override o1 o2)]
        [(and (boolean? o1) (list? o2)) (broadcast or-override o2 o1)]
        [(and (string? o1) (string? o2)) (string-append o1 o2)]
        [(and (list? o1) (string? o2)) (broadcast string-append o1 o2)]
        [(and (string? o1) (list? o2)) (elementwise reverse-str (broadcast string-append (elementwise reverse-str o2) (reverse-str o1)))] ; maybe wrong
        [(and (list? o1) (list? o2)) (append o1 o2)]
       )
   )
 )

; subtraction
(define (eval-sub args env)
  (let
      (
        [o1 (eval (first args) env)]
        [o2 (eval (second args) env)]
       )
      (cond
        [(and (number? o1) (number? o2)) (- o1 o2)]
        [(and (list? o1) (number? o2)) (broadcast - o1 o2)]
        [(and (number? o1) (list? o2)) (broadcast * (broadcast - o2 o1) -1)] ; maybe wrong
       )
   )
 )

;MUL
(define (eval-mul args env)
  (let
      (
        [o1 (eval (first args) env)]
        [o2 (eval (second args) env)]
       )
      (cond
        [(and (number? o1) (number? o2)) (* o1 o2)]
        [(and (list? o1) (number? o2)) (broadcast * o1 o2)]
        [(and (number? o1) (list? o2)) (broadcast * o2 o1)]
        [(and (boolean? o1) (boolean? o2)) (and o1 o2)]
        [(and (list? o1) (boolean? o2)) (broadcast and-override o1 o2)]
        [(and (boolean? o1) (list? o2)) (broadcast and-override o2 o1)]
        [else (raise "invalid mul")]
       )
   )
 )

;DIV
(define (eval-div args env)
  (let
      (
        [o1 (eval (first args) env)]
        [o2 (eval (second args) env)]
       )
      (cond
        [(and (number? o1) (number? o2)) (/ o1 o2)]
        [(and (list? o1) (number? o2)) (broadcast / o1 o2)]
        [(and (number? o1) (list? o2)) (broadcast * (elementwise reverse-num o2) o1)] ; maybe wrong
       )
   )
 )

(define (reverse-num x) (/ 1 x)) ; maybe useless
(define (reverse-str x) (list->string (reverse (string->list x)))) ; maybe useless

;MINUS (-x)
(define (eval-minus args env)
  (let
      ([o1 (eval (first args) env)])
      (cond
        [(number? o1) (- 0 o1)]
        [(boolean? o1) (not o1)]
        [(list? o1) (elementwise - o1)]
        [else (raise "invalid minus")]
       )
   )
 )


(define (eval-number args env) (first args))
(define (eval-null args env) 'n)
(define (eval-variable args env) (get-variable (first args) env))
(define (eval-true args env) #t)
(define (eval-false args env) #f)
(define (eval-string args env) (string-trim (first args) "\"")) ; maybe wrong
(define (eval-list args env)
  (cond
    [(eq? args 'emptylist) (list)]
    [(eq? (first args) 'aslist) (list (eval (second args) env))]
    [(eq? (first args) 'append) (append (list (eval (second args) env)) (eval-list (third args) env))]
   )
 )

; get list[idx]
(define (list-get-item mylist index)
  (if (= index 0)
       (first mylist)
       (list-get-item (rest mylist) (- index 1))
    )
 )

(define (access mylist exps env)
  (if (empty? exps)
      mylist
      (access (list-get-item mylist (eval (first exps) env)) (rest exps) env)
   )
 )

(define eval-com-list '(gt lt beq bne sub add mul div minus number null variable true false string list access parenthese))

; list.contains(item)
(define (list-contains mylist item)
  (if (empty? mylist)
       #f
       (if (eq? (first mylist) item)
          #t
          (list-contains (rest mylist) item)
         )
    )
 )

(define (cleanup-exps exps)
   (cond
     [(empty? exps) (list)]
     [(list-contains eval-com-list (first exps)) (list exps)]
     [else (append (cleanup-exps (first exps)) (cleanup-exps (rest exps)))]
    )
 )

(define (eval-access args env)
   (access (get-variable (first args) env) (cleanup-exps (second args)) env)
 )







(define (eval exp env)
  (cond
      [
        (and (list? exp) (not (empty? exp)) (list-contains eval-com-list (first exp)))
           (case (first exp)
             ['gt (eval-gt (rest exp) env)]
             ['lt (eval-lt (rest exp) env)]
             ['beq (eval-beq (rest exp) env)]
             ['bne (eval-bne (rest exp) env)]
             ['sub (eval-sub (rest exp) env)]
             ['add (eval-add (rest exp) env)]
             ['mul (eval-mul (rest exp) env)]
             ['div (eval-div (rest exp) env)]
             ['minus (eval-minus (rest exp) env)]
             ['number (eval-number (rest exp) env)]
             ['null (eval-null (rest exp) env)]
             ['variable (eval-variable (rest exp) env)]
             ['true (eval-true (rest exp) env)]
             ['false (eval-false (rest exp) env)]
             ['string (eval-string (rest exp) env)]
             ['list (eval-list (second exp) env)]
             ['access (eval-access (rest exp) env)]
             ['parenthese (eval (second exp) env)]
            )
      ]
      [else exp]
    )
 )

(define (extract-commands code)
   (cond
     [(empty? code) (list)]
     [(list? (first code)) (append (extract-commands (first code)) (rest code))]
     [else (list code)]
         
    )
 )



(define (run parse-tree env)
  (let
      ([com (first parse-tree)])
      (cond
         [(not (empty? parse-tree))
          (case (first com)
            ['assign (run (rest parse-tree) (set-variable (second (rest com)) (list (second com) env) env))]
            ['return (eval (second com) env)]
            ['if
             (cond
                  [(eval (second com) env) (run (append (extract-commands (second (rest com))) (rest parse-tree)) env)]
               [else (run (append (extract-commands (third (rest com))) (rest parse-tree)) env)])]
            ['while
             (cond
               [(eval (second com) env) (run (append (extract-commands (second (rest com))) parse-tree) env)]
               [else (run (rest parse-tree) env)])])
         ]
        )
   )
 )

;test
(define (main input-path)
    (define lex-this (lambda (lexer input) (lambda () (lexer input))))
    (define my-lexer (lex-this basic-lexer (open-input-string (string-join (map ~a (append (file->lines "builtin.txt") (file->lines input-path))) " "))))
    (define parse-tree (extract-commands (let ((parser-res (basic-parser my-lexer))) parser-res)))

    (display (run parse-tree initial-env))
)

(main "test.txt")
