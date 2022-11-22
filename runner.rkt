#lang racket
(require "other.rkt")
(require "parser.rkt")


(define resolve_scope;((a 1) (b 2) (c 5)), it gives two kinds of result. found return a value
  ; not found return #false
  (lambda (scope varname)
    (cond
      ((null? scope) #false)
      ((equal? (caar scope) varname) (cadar scope))
      (else (resolve_scope (cdr scope) varname))
      )
    )
)
  
  

;(car(cdr(caar(cdr scope)

(define resolve_env
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((null? (car environment)) (resolve_env (cdr environment) varname))
      ((equal? 'global (car (car environment))) (resolve_scope (cdr (car environment)) varname))
      (else (let ((resolved_result (resolve_scope (car environment) varname)))
              (if (equal? resolved_result #false)
                  (resolve_env (cdr environment) varname)
                  resolved_result
                  )
              )
       )
      )
    )
  )
                             

(define extend-scope
  (lambda (list-of-varname list-of-value scope)
    (cond
      ((null? list-of-varname)scope);do nothing if the list is null
      ((null? list-of-value)scope);do nothing if the list is null
      (else (extend-scope(cdr list-of-varname)(cdr list-of-value)
       (cons (list (car list-of-varname)
                   (car list-of-value))
             scope)))
      )
    )
  )

(define push_scope_to_env
  (lambda (list-of-varname list-of-value env)
    ;construct a new scope based on list of varnames and list of values
    (let ((new_scope (extend-scope list-of-varname list-of-value '()))
          (pop_off_env (pop_env_to_global_scope env))) ;pop off scopes on top of global scope in environment
      (cons new_scope pop_off_env) ;concate the new scope to the global scope environment
      )
    )
  )

;remove all scopes on top of global scope
(define pop_env_to_global_scope
  (lambda (env)
    (cond
      ((null? env) #false)
      ((equal? (length env) 1)
       (if (equal? (car (car env)) 'global) env
           #false))
      (else (pop_env_to_global_scope (cdr env)))
      )
    )
  )

;add name value pairs to the local scope
(define extend_local_scope
  (lambda (list-of-varname list-of-value env)
    (cond
      ((null? env) #false)
      ;check the first scope is local scope or not
      ((equal? (caar env) 'global) (push_scope_to_env list-of-varname list-of-value env))
      ;use extend_scope function to add new variables into the local scope
      (else (cons (extend-scope list-of-varname list-of-value (car env))
                  (pop_env_to_global_scope env)))
     )
    )
  )


(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) '())

      ((equal?(car parsed-code) 'num-exp)
       (cadr parsed-code));(num-exp 22)

      ((equal? (car parsed-code) 'var-exp)
       (resolve_env env (cadr parsed-code)))
      
      ((equal? (car parsed-code) 'bool-exp)(run-bool parsed-code env))
      
      ((equal? (car parsed-code) 'math-exp)(run-math parsed-code env));(math-exp op (neo-exp) (neo-exp))
      
      ((equal? (car parsed-code) 'ask-exp) (run-ask parsed-code env))
      
      ((equal? (car parsed-code) 'func-exp)(run-func parsed-code env))

      ((equal? (car parsed-code) 'let-exp)
       (run-let-exp parsed-code env))
      
      ((equal? (car parsed-code) 'print-exp)
       (run-print-exp (cadr parsed-code) env))

      ((equal? (car parsed-code) 'assign-exp)
       (run-assign-exp (elementAt parsed-code 1)
                       (run-neo-parsed-code (elementAt parsed-code 2) env)
                       env))
      
      ((equal? (car parsed-code) 'block-exp)
       (run-block-exp (cdr parsed-code) env))

       
      (else (run-neo-parsed-code
            (cadr parsed-code) ;function expression
            (push_scope_to_env (cadr (cadr (cadr parsed-code)))
                                (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code))
                                env
                                )
             );environment scope update
         )            
        )
       )
      )
    
  


         
       


(define run-bool
  (lambda(parsed-code env)
    (run-bool-exp (cadr parsed-code);run bool-exp
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
  )


(define run-func
  (lambda(parsed-code env)
     (run-neo-parsed-code (cadr (caddr parsed-code)) env))
  )

(define run-ask
  (lambda (parsed-code env)
    (if (run-neo-parsed-code (cadr parsed-code) env)
           (run-neo-parsed-code (caddr parsed-code) env)
           (run-neo-parsed-code (cadddr parsed-code) env)))
)

(define run-math
  (lambda(parsed-code env)
     (run-math-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env))))
   

(define run-math-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '+) (+ num1 num2)) ;+ 1 1)
      ((equal? op '-) (- num1 num2))  ;- 1 1
      ((equal? op '*) (* num1 num2)) ;* 1 1
      ((equal? op '/) (/ num1 num2))  ;/ 1 1 float division
      ((equal? op '//) (quotient num1 num2))  ;// 1 1 interger division
      ((equal? op '%) (modulo num1 num2))  ;% 1 1 modulo
      (else #false)
      )
    )
  )



(define run-bool-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '>) (> num1 num2))
      ((equal? op '<) (< num1 num2))
      ((equal? op '>=) (>= num1 num2))
      ((equal? op '<=) (<= num1 num2))
      ((equal? op '==) (= num1 num2))
      ((equal? op '&&) (and num1 num2))
      ((equal? op '||) (or num1 num2))
      ((equal? op '!=) (not(= num1 num2)))
      (else (not num1))
      )
    )
  )

(define run-let-exp
  (lambda (parsed-code env)
    ;((a (num-exp 7)) (b (var-exp a)) (x (var-exp b))) > ((a 7) (b 7) (x 7))
    ;(a (num-exp 7)) -> (a 7) < (list (car code) (run-neo-parsed-code (cadr code) env))) 
    (let* ((new_env (cascade-update-env (elementAt parsed-code 1) env))
          (body (elementAt parsed-code 2)))
      (run-neo-parsed-code body new_env)
    )
  )
)

(define run-print-exp
  (lambda(parsed-code env)
     (string-append "**screen** " (number->string (run-neo-parsed-code parsed-code env)))))
  


(define run-assign-exp
  (lambda (varname value env)
    (cond
      ((null? env) #false)
      ((equal? (caar env) 'global)
       (cons (list (list varname value)) env)); 
      (else (let*
          ((new-local-scope (update-scope varname value (car env)))
           (under-env (cdr env)))
        (cons new-local-scope under-env))
      )
      )
    )
  )

(define run-block-exp
  (lambda(parsed-list-exp env)
    (cond
      ((null? parsed-list-exp) '())
      ((equal? (caar parsed-list-exp) 'assign-exp)
       (run-block-exp
        (cdr parsed-list-exp)
        (run-assign-exp
         (cadr(car parsed-list-exp))
         (run-neo-parsed-code (elementAt(car parsed-list-exp)2)env)
         env)))
      (else
       (cons (run-neo-parsed-code(car parsed-list-exp)env)
             (run-block-exp (cdr parsed-list-exp) env))
       )
      )
    )
  )

(define update-scope
  (lambda(varname value scope)
    (letrec (
           (check-varname-in-pair(lambda(pair)(equal? (car pair) varname)))
           (check-varname-in-scope
            (lambda (curr_scope)
              (cond
                ((null? curr_scope)#false)
                (else
                 (if(check-varname-in-pair(car curr_scope))#true
                    (check-varname-in-scope(cdr curr_scope))
                    )
                 )
                )
              )
            )
           )
      
    (cond
      ((null? scope)(cons(list varname value) scope))
      ((equal? varname(caar scope)) (cons (list varname value)(cdr scope)))
      (else
       (if (check-varname-in-scope scope)
           (cons (car scope)(update-scope varname value(cdr scope)))
           (cons(list varname value) scope)
           )
       )
      )
      )
)
  )
(define cascade-update-env
  (lambda (parsed-scope env)
    (if (null? parsed-scope) env
        (let* (
               ;1. what is the local scope: (((a 7)) (global (a 1) (b 2) (c 5)))
               ;1.1 there is only one global scope there, so local scope should be '()
               ;1.2 there is a scope on top of global scope, that is the local scope
               (local-scope (if (equal? (car (car env)) 'global)
                                '()
                                (car env)))
               ;2. the global scope
               (global-scope-env (pop_env_to_global_scope env));((global (a 1)...)
               ;3. update the local scope
               (first-name-value-pair (car parsed-scope));((a 7)...)
               (new-local-scope (cons
                                 (list
                                  (car first-name-value-pair)
                                  (run-neo-parsed-code (cadr first-name-value-pair) env))
                                 local-scope))
               ;4. concate updated local scope on top of global scope to form the new environment
               (new_env (cons new-local-scope global-scope-env))
               )
          (cascade-update-env (cdr parsed-scope) new_env)
          )
      )
    )
  )


(provide(all-defined-out))