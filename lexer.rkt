#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define basic-lexer
           (lexer
            ("+"                            (token-PLUS))
            ("-"                            (token-MINUS))
            ("*"                            (token-MULT))
            ("/"                            (token-DIV))
            ("="                            (token-EQUALS))
            ("=="                           (token-BEQ))
            ("!="                           (token-NEQ))
            (">"                            (token-GT))
            ("<"                            (token-LT))
            ("("                            (token-LP))
            (")"                            (token-RP))
            ("["                            (token-LB))
            ("]"                            (token-RB))
            (","                            (token-COMMA))
            (";"                            (token-SEMIC))
            ("return"                       (token-RETURN))
            ("if"                           (token-IF))
            ("then"                         (token-THEN))
            ("else"                         (token-ELSE))
            ("end"                          (token-END))
            ("while"                        (token-WHILE))
            ("do"                           (token-DO))
            ("true"                         (token-TRUE))
            ("false"                        (token-FALSE))
            ("null"                         (token-NULL))
            ("switch"                       (token-SWITCH))
            ("case"                        (token-CASE))

            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUMBER (string->number lexeme)))
            ((:+ alphabetic)                                                                              (token-VARIABLE (string->symbol lexeme)))
            (whitespace (basic-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens value-tokens
  (NUMBER VARIABLE)
 )

(define-empty-tokens empty-tokens
  (PLUS MINUS MULT DIV EQUALS BEQ NEQ GT LT
   LP RP LB RB COMMA  SEMIC RETURN IF THEN ELSE
   END WHILE DO TRUE FALSE NULL SWITCH CASE
   EOF)
 )


;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this basic-lexer (open-input-string "heLLo switch 010.4 if")))
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
