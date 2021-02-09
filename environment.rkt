#lang racket

(define (empty-env) (list 'empty-env))

(define (extend-env var val env) (list 'extend-env var val env))

(define (apply-env env search-var)
    (cond
      [(eqv? (car env) 'empty-env)
       (report-no-binding-found search-var)]
      [(eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr  env)))
         (if (string=? search-var saved-var)
             saved-val
             (apply-env saved-env search-var)))]
      [else (report-invalid-env env)]
      ))

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (error 'apply-env "Bad environment: ~s" env)))

(define env '(extend-env a 6 (extend-env b 8 (extend-env c -4 (empty-env)))))
(provide (all-defined-out))