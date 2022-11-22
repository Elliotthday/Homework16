#lang racket
(require "other.rkt")
(require "parser.rkt")
(require "runner.rkt")
;(define env '((a 1) (b 2) (c 5)))
(define env '((global (a 1) (b 2) (c 5)(p 10))))
;(define sample-code '(call (function()(ask (bool > a b)(math - 1 2)(math - 1 2)))(a)))
;(define parsed-neo-code (neo-parser sample-code))
;(display parsed-neo-code)
;(display "\n")
;(run-neo-parsed-code parsed-neo-code env)
;(define sample-code '(local-vars ((a 7) (b a) (x b)) (math + x a)))



;(define sample-code '(local-vars ((p 10)) (math / a p)))
;(define sample-code '(block (print a) (assign x 8) (assign y (math * x 2)) (print y) (assign z (math + b y)) (print z)))


(define sample-code '(block (print a) (assign x 8) (assign y (math * x 2)) (print y) (assign z (math + b y)) (print z)))


(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)




