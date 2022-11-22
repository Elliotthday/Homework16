#lang racket
(require "other.rkt")

(define neo-parser
  (lambda (neo-code)
    (cond
      ((number? neo-code) (list 'num-exp neo-code))

      ((symbol? neo-code) (list 'var-exp neo-code))

      ((equal? (car neo-code) 'bool)(bool-parser neo-code))
      ;Branch to parse (bool op num1 num2) into (bool-exp op(neo-exp) (neo-exp))
       
      
      ((equal? (car neo-code) 'math)(math-parser neo-code))
      ;(math op num1 num2) > (math-exp op (neo-exp) (neo-exp))
      ((equal? (car neo-code) 'ask)(ask-parser neo-code));Decision branch
       
      ;(function (x y z,...) x)
      ((equal? (car neo-code) 'function)(func-parser neo-code))
      
      ((equal? (car neo-code) 'call)(call-parser neo-code))

      ((equal? (car neo-code) 'local-vars)(let-parser neo-code))

      ((equal? (car neo-code) 'print) (list 'print-exp(neo-parser (cadr neo-code))))

      ((equal? (car neo-code) 'assign)
       (list 'assign-exp (cadr neo-code) (neo-parser (caddr neo-code))))

      ((equal? (car neo-code) 'block)
       (cons 'block-exp (neo-parser (cdr neo-code))))
                                    
      (else (map neo-parser neo-code)) ;runs neo-parser on everything in the neo-code 
      )
    )
  )



(define math-parser 
  (lambda (neo-code)
(list 'math-exp (cadr neo-code)
             (neo-parser (caddr neo-code))
             (neo-parser (cadddr neo-code))))

  )

(define bool-parser
 (lambda (neo-code)
   (if (equal? (length neo-code) 3)
            (list 'bool-exp (elementAt neo-code 1) (neo-parser (caddr neo-code)) '())
        (cons 'bool-exp (cons (cadr neo-code) (map neo-parser (cddr neo-code))))))
  )
(define ask-parser
  (lambda (neo-code)(cons 'ask-exp
             (map neo-parser (cdr neo-code))))
  )
  
(define func-parser
  (lambda (neo-code)
    (list 'func-exp
             (list 'params (cadr neo-code));passes everything in function(x y z) instead of caadr which would pass just x
             (list 'body-exp (neo-parser (caddr neo-code)))))
  
)
(define call-parser
  (lambda(neo-code)
     (list 'app-exp
             (neo-parser (cadr neo-code))
             (neo-parser (caddr neo-code))))
  )



(define let-parser
   (lambda (neo-code)
    (list 'let-exp
          (map (lambda (pair) (list (car pair) (neo-parser (elementAt pair 1))))
               (elementAt neo-code 1))
           (neo-parser (elementAt neo-code 2)))
    )
  )
  
  






    
(provide(all-defined-out))