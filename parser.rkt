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
                  [(keyword)               (list 'begin $1)]
                  [(command SEMIC keyword) (appened $1 (list $3))]
                )

               (keyword
                  [(if_statement)         $1]
                  [(assignment_statement) $1]
                  [(while_statement)      $1]
                  [(return)               $1]
                )

               (while_statement
                  [(WHILE exp DO command END) ]  ; TODO
                )

               (if_statement
                  [(IF exp THEN command ELSE command END) (list 'if $2 $4 $6)]
                )

               (assignment_statement
                  [(VARIABLE EQUALS exp) (list 'set! $2 $4)]  ;maybe wrong
                )

               (return
                  [(RETURN exp) ]  ; TODO
                )

               (exp
                  [(aexp) $1]
                  [(aexp GT aexp)  (list '> $1 $3)]
                  [(aexp LT aexp)  (list '< $1 $3)]
                  [(aexp BEQ aexp) (list '= $1 $3)]
                  [(aexp BNE aexp) (list 'not (list '= $1 $3))]
                )

               (aexp
                  [(bexp)             $1]
                  [(bexp MINUS aexp) (list '- $1 $3)]
                  [(bexp PLUS aexp)  (list '+ $1 $3)]
                )

               (bexp
                  [(cexp)            $1]
                  [(cexp MULT bexp) (list '* $1 $3)]
                  [(cexp DIV bexp)  (list '/ $1 $3)]
                )

               (cexp
                  [(MINUS cexp)         (list '- 0 $2)]
                  [(LP exp RP)          (list $2)]
                  [(NUMBER)              $1]
                  [(NULL)                $1]
                  [(VARIABLE)            $1]
                  [(BOOLEAN)             $1]
                  [(STRING)              $1]
                  [(list)                $1]
                  [(VARIABLE listMember) $1]
                )

               (list
                  [(listValues) (list $1)]
                  [(LB RB)       null]
                )

               (listValues
                  [(exp)                  (list $1)]
                  [(exp COMMA listValues) (append (list $1) $3)]
                )

               (listMember
                  [(LB exp RB)            (list $2)]
                  [(LB exp RB listMember) (append (list $2) $4)]
                )
              )
            )
 )

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2+ 3 +   4")))
(let ((parser-res (basic-parser my-lexer))) parser-res)

