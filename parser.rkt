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

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2+ 3 +   4")))
(let ((parser-res (basic-parser my-lexer))) parser-res)

