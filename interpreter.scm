;; program := (unit*)
;; unit := toplevel-define
;;      |  exp

;; toplevel-define := (define var exp)

;; exp := numbers 0,1,2...
;;     |  booleans #t #f
;;     |  variable symbols that are not keywords
;;     |  (let ([var exp]*) exp)
;;     |  (lambda (var*) exp)
;;     |  (exp exp*)
;;     |  (cond (exp exp)* (else exp))
;;     |  (if exp exp exp)


;; utilities

(define proper-list-of-length
  (lambda (lst n)
    (cond [(and (pair? lst) (> n 0))
           (proper-list-of-length (cdr lst) (1- n))]
          [(and (null? lst) (zero? n))
           #t]
          [else
           #f])))

(define proper-list-of-length-at-least
  (lambda (lst n)
    (cond [(and (pair? lst) (> n 0))
           (proper-list-of-length-at-least (cdr lst) (1- n))]
          [(and (list? lst) (zero? n))
           #t]
          [else
           #f])))


(define keywords
  '(define lambda if else quote unquote))

(define mem-of?
  (lambda (target list)
    (find (lambda (x)
            (eq? x target))
          list)))

(define empty-env '())

(define default-env
  `(
    (+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)
    (> . ,>)
    (< . ,<)
    (= . ,=)

    ))

(define look-up-env
  (lambda (env target)
    (cond [(pair? env)
           (let ([head (car env)])
             (cond [(pair? head)
                    (if (eq? (car head) target)
                        (cdr head)
                        (look-up-env (cdr env) target))]
                   [else
                    (errorf 'look-up-env "Item: ~s in environment is not valid" head)]))]
          [(null? env)
           (errorf 'look-up-env "Cannot find variable ~s" target)]
          [else
           (errorf 'look-up-env "env is not a list")])))

(define add-to-env
  (lambda (env target)
    (cons target env)))

;; predicates and accessors
(define is-constant?
  (lambda (exp)
    (or (number? exp)
        (boolean? exp))))

(define is-variable?
  (lambda (exp)
    (and (symbol? exp)
         (not (mem-of? exp keywords)))))

(define is-let?
  (lambda (exp)
    (and (proper-list-of-length exp 3)
         (eq? (list-ref exp 0) 'let))))

(define let-1
  (lambda (exp)
    (list-ref exp 1)))

(define let-2
  (lambda (exp)
    (list-ref exp 2)))

(define is-application?
  (lambda (exp)
    (proper-list-of-length-at-least exp 1)))

(define applications-1
  (lambda (exp)
    (list-ref exp 0)))

(define applications-2
  (lambda (exp)
    (cdr exp)))

(define is-lambda?
  (lambda (exp)
    (and (proper-list-of-length exp 3)
         (eq? (list-ref exp 0) 'lambda))))

(define is-cond?
  (lambda (exp)
    (and (proper-list-of-length-at-least exp 2)
         (eq? (list-ref exp 0) 'cond))))

(define cond-1
  (lambda (exp)
    (cdr exp)))

(define is-if?
  (lambda (exp)
    (and (proper-list-of-length exp 4)
         (eq? (list-ref exp 0) 'if))))

(define if-1
  (lambda (exp)
    (list-ref exp 1)))

(define if-2
  (lambda (exp)
    (list-ref exp 2)))

(define if-3
  (lambda (exp)
    (list-ref exp 3)))


(define interpret
  (lambda (exp env)
    (cond [(is-constant? exp)
           exp]
          [(is-variable? exp)
           (look-up-env env exp)]
          [(is-let? exp)
           (let ([newenv
                  (fold-left
                   (lambda (res item)
                     (cond [(pair? item)
                            (add-to-env res `(,(car item) . ,(interpret (cadr item) env)))]
                           [else
                            (errorf 'interpret "let-construct ~s not a valid binding" item)]))
                   env
                   (let-1 exp))])
             (interpret (let-2 exp) newenv))]
          [(is-if? exp)
           (if (interpret (if-1 exp) env)
               (interpret (if-2 exp) env)
               (interpret (if-3 exp) env))]
          [(is-cond? exp)
           (letrec ([body (cond-1 exp)]
                    [helper
                     (lambda (body)
                       (cond [(pair? body)
                              (let ([head (car body)]
                                    [tail (cdr body)])
                                (if (null? tail)
                                    (if (eq? (car head) 'else)
                                        (interpret (cadr head) env)
                                        (errorf 'interpret "cond-exp: last clause ~s not else" head))
                                    (if (interpret (car head) env)
                                        (interpret (cadr head) env)
                                        (helper tail))))]
                             [else
                              (errorf 'interpret "cond-exp: ~s not a valid expression" exp)]))])
             (helper body))]
          [(is-lambda? exp)
           exp]
          [(is-application? exp)
           (let ([func
                  (interpret (applications-1 exp) env)]                 
                 [args
                  (reverse
                   (fold-left
                    (lambda (res item)
                      (cons (interpret item env) res))
                    '()
                    (applications-2 exp)))])
             (apply func args))]
          [else
           (errorf 'interpret "~s: TO BE IMPLEMENTED" exp)])))

;; test

(define start-trace
  (lambda ()
    (trace interpret look-up-env fold-left)))

(define end-trace
  (lambda ()
    (untrace interpret look-up-env fold-left)))

(define test-framework
  (lambda (candidate test-suite)
    (fold-left
     (lambda (res item)
       (if (equal? (candidate (car item) default-env)
                   (cadr item))
           res
           (cons item res)))
     '()
     test-suite)))

(test-framework interpret
                '((1 1)
                  (#t #t)
                  (#f #f)
                  (42 42)
                  ((let ([a 5]) a) 5)
                  ((let ([a 5]
                          [b 6]
                          [c 3])
                      b)
                   6)
                  ((+ 1 2)
                   3)
                  ((+ (+ 1 2) 3)
                   6)
                  ((+ (+ 6 7) (- 8 2)) 19)
                  ((let ([a 5])
                     (+ 6 (- a 2))) 9)
                  ((let ([b (let ([a 10]) (+ 1 a))])
                     (- b 2)) 9)
                  ((let ([a 10])
                     (let ([a (+ a 1)])
                       a)) 11)
                  ((let ([a 10])
                     (let ([a 20])
                       a)) 20)
                  ((cond [#t 1] [else 2]) 1)
                  ((cond [(> 1 2) 3] [(< 1 2) 5] [else 1]) 5)
                  ((cond [(> 1 2) 3] [(< 1 0) 5] [else 1]) 1)
                  ((if (> 1 2) 1 2) 2)
                  (((lambda (x) (if (> x 0) x 0)) 5) 5)
                  ))



            
                    
;; (interpret 1 empty-env)
;; (interpret #t empty-env)
;; (interpret 42 empty-env)
;; (interpret #f empty-env)
;; (interpret 'k '((a . 42) (k . 52)))
;; (interpret '(let ([a 5]) a) empty-env)
;; (interpret '(let ([a 5]
;;                   [b 6]
;;                   [c 3])
;;               b)
;;            empty-env)


          
