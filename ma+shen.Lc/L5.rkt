#lang racket

;;done
(define key-words
  (set 'print 'new-array 'aref 'aset 'alen))
(define biop
  (set '+ '- '* '< '<= '=))
(define pred
  (set 'number? 'a?))
(define prim
  (set-union biop pred key-words))

(define reserved-words
  (set 'print 'new-array 'aref 'aset 'alen 
       'let 'letrec 'if 'new-tuple 'begin 'lambda))

;;translate L5-program
(define (label? expr)
  (if (symbol? expr)
      (if (not (reserved-words? expr))
          (match (symbol->string expr)
            [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$") #t]
            [_ #f])
          #f)
      #f))

(define (variable? expr)
  (if (symbol? expr)
      (if (not (reserved-words? expr))
          (match (symbol->string expr)
            [(regexp #rx"^[a-zA-Z_-][a-zA-Z_0-9-]*$") #t]
            [_ #f])
          #f)
      #f))

(define (reserved-words? expr)
  (or (set-member? reserved-words expr)
       (set-member? prim expr)))

(define (translate-L5 expr)
  (cond
    [(label? expr) (string->symbol (string-append ":l5c_" (substring (symbol->string expr) 1)))]
    [(variable? expr) (string->symbol (string-append "l5c_" (symbol->string expr)))]
    [(pair? expr) (cons (translate-L5 (car expr))
                        (translate-L5 (cdr expr)))]
    [else expr]))

;;done
(define var-counter -1)
(define label-counter -1)

(define var-prefix 'var_)
(define label-prefix ':label_)

(define (fresh-var)
  (string->symbol (string-append 
                   (symbol->string var-prefix)
                   (number->string (v-counter)))))

(define (fresh-label)
  (string->symbol (string-append 
                   (symbol->string label-prefix)
                   (number->string (l-counter)))))

(define (v-counter)
  (begin (set! var-counter
               (add1 var-counter))
         var-counter))

(define (l-counter)
  (begin (set! label-counter
               (add1 label-counter))
         label-counter))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;done optimization
(define lambda-function-lst '())
(define (compile-e e)
  (match e
    [`(lambda (,x ...) ,func-e)
     (let ([func-name (fresh-label)]
           [free-vars (find-free-vars func-e x)])
       (set!
        lambda-function-lst
        (cons
         (lambda-to-function func-name free-vars x (compile-e func-e))
         lambda-function-lst))
       `(make-closure ,func-name (new-tuple ,@free-vars)))]
    [`(let ([,x ,e1]) ,e2)
     `(let ([,x ,(compile-e e1)])
        ,(compile-e e2))]
    [`(letrec ([,x ,e1]) ,e2)
     (if (and (list? e2)
              (list? e1)
              (equal? x (first e2))
              (equal? 'lambda (first e1)))
         (let ([e1-free-vars (find-free-vars (third e1) (cons 
                                                         x (second e1)))]
               [e2-free-vars (find-free-vars e2 (list x))])
           (if (and (null? e1-free-vars)
                    (null? e2-free-vars))
               (let ([new_label (fresh-label)])
                 (set!
                  lambda-function-lst
                  (cons
                   `(,new_label (,@(second e1)) ,(let-replacement (third e1) x new_label))
                   lambda-function-lst))
                 `(,new_label ,(let-replacement (second e2) x new_label)))
               `(let ([,x (new-tuple 0)])
                  (begin 
                    (aset ,x 0 ,(compile-e (let-replacement e1 x `(aref ,x 0))))
                    ,(compile-e (let-replacement e2 x `(aref ,x 0)))))))
         `(let ([,x (new-tuple 0)])
            (begin 
              (aset ,x 0 ,(compile-e (let-replacement e1 x `(aref ,x 0))))
              ,(compile-e (let-replacement e2 x `(aref ,x 0))))))]
    [`(if ,e1 ,e2 ,e3)
     `(if ,(compile-e e1)
          ,(compile-e e2)
          ,(compile-e e3))]
    [`(new-tuple ,e ...)
     `(new-tuple ,@(map compile-e e))]
    [`(begin ,e1 ,e2)
     `(begin 
        ,(compile-e e1)
        ,(compile-e e2))]
    [`(,e-0 ,e ...)
     (if (set-member? prim e-0)
         `(,e-0 ,@(map compile-e e))
         (let ([func-name-without-colon (fresh-var)])
           `(let ([,func-name-without-colon ,(compile-e e-0)])
              ((closure-proc ,func-name-without-colon)
               (closure-vars ,func-name-without-colon)
               ,@(map compile-e e)))))]
    [(? symbol?)
     (if (set-member? prim e)
         (compile-e (new-lambda-expression e))
         e)]
    [(? number?) e]))

;;find free variables
(define (find-free-vars expr args (free-vars-lst '()))
  (match expr
    [`(lambda (,x ...) ,func-e)
     (find-free-vars func-e (set-union args x) free-vars-lst)]
    [(or `(let ([,x ,e1]) ,e2)
         `(letrec ([,x ,e1]) ,e2))
     (set-union
      (find-free-vars e1 args free-vars-lst)
      (find-free-vars e2 (cons x args) free-vars-lst))]
    [`(if ,e1 ,e2 ,e3)
     (set-union
      (find-free-vars e1 args free-vars-lst)
      (find-free-vars e2 args free-vars-lst)
      (find-free-vars e3 args free-vars-lst))]
    [`(begin ,e1 ,e2)
     (set-union
      (find-free-vars e1 args free-vars-lst)
      (find-free-vars e2 args free-vars-lst))]
    [`(,e-0 ,e ...)
     (foldl set-union
            '()
            (map (λ (expr) (find-free-vars expr args free-vars-lst)) 
                 (if (set-member? prim e-0)
                     e
                     (cons e-0 e))))]
    [(? symbol?)
     (if (set-member? args expr)
         free-vars-lst
         (cons expr free-vars-lst))]
    [(? number?) free-vars-lst]))                        

;;replace variables recursively
(define (replace-var expr var new-var)
  (cond
    [(equal? var expr) new-var]
    [(pair? expr) (cons (replace-var (car expr) var new-var)
                        (replace-var (cdr expr) var new-var))]
    [else expr]))

(define (let-replacement expr var new-var)
  (match expr 
    [(or `(let ([,x ,e1]) ,body-e)
         `(letrec ([,x ,e1]) ,body-e))
     (if (equal? x var) 
         expr
         (replace-var expr var new-var))]
    [else (replace-var expr var new-var)]))

;;turn it into lambda expression and then closure convert it.
(define (new-lambda-expression prim-e)
  (cond
    [(or (set-member? biop prim-e) 
         (equal? prim-e 'aref)
         (equal? prim-e 'new-array))
     `(lambda (x y) (,prim-e x y))]
    [(or (set-member? pred prim-e)
         (equal? prim-e 'print)
         (equal? prim-e 'alen))
     `(lambda (x) (,prim-e x))]
    [else
     `(lambda (x y z) (aset x y z))]))

;;add function transforming from lambda
(define (lambda-to-function func-name free-vars args expr)
  `(,func-name (vars-tup ,@args) ,(let-free-vars free-vars expr)))

(define (let-free-vars free-vars expr (counter 0))
  (begin
    (if (> (length free-vars) 0)
        `(let ([,(first free-vars) (aref vars-tup ,counter)])
           ,(let-free-vars (rest free-vars) expr (+ counter 1)))
        expr)))

;;top level
(define (compile-L5 expr)
  (set! lambda-function-lst '())
  (set! var-counter -1)
  (set! label-counter -1)
  (cons (compile-e (translate-L5 expr)) lambda-function-lst))

;;read file 
(define in (call-with-input-file "filename.in" read))

(define (read-from-file filename)
  (call-with-input-file filename read))

;;provide compile-L5 
(provide compile-L5)
