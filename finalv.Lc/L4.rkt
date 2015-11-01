#lang plai

;;done
(define-type context
  [let-ctxt (x var?)
            (b L4-e?)
            (k context?)]
  [if-ctxt (t L4-e?)
           (e L4-e?)
           (k context?)]
  [fun-ctxt (a (listof L4-e?))
            (k context?)]
  [arg-ctxt (f val?)
            (sub-norm (listof L4-e?))
            (sub-remain (listof L4-e?))
            (k context?)]
  [no-ctxt])

;;translate L4-program
(define (label? expr)
  (if (symbol? expr)
      (if (not (reserved-words? expr))
          (match (symbol->string expr)
            [(regexp #rx"^:[a-zA-Z_-][a-zA-Z_0-9-]*$") #t]
            [_ #f])
          #f)
      #f))

(define (var? expr)
  (if (symbol? expr)
      (if (not (reserved-words? expr))
          (match (symbol->string expr)
            [(regexp #rx"^[a-zA-Z_-][a-zA-Z_0-9-]*$") #t]
            [_ #f])
          #f)
      #f))

(define reserved-words
  (set 'new-array 'new-tuple 'aref 'aset 'alen 'begin
       'print 'make-closure 'closure-proc 'closure-vars 'let 'if
       '+ '- '* '< '<= '=
       'number? 'a?))

(define (reserved-words? expr)
  (set-member? reserved-words expr))

(define (translate-L4 expr)
  (cond
    [(label? expr) (string->symbol (string-append ":l5c_" (substring (symbol->string expr) 1)))]
    [(var? expr) (string->symbol (string-append "l5c_" (symbol->string expr)))]
    [(pair? expr) (cons (translate-L4 (car expr))
                        (translate-L4 (cdr expr)))]
    [else expr]))

(define (val? s)
  (or (number? s) (symbol? s)))

(define pred-set (set 'a? 'number?))
(define biop-set (set '+ '- '* '< '<= '=))

(define (biop? e)
  (set-member? biop-set e))
(define (pred? e)
  (set-member? pred-set e))

(define var-counter -1)

(define var-prefix 'L4_lbl_)

(define (fresh-var)
  (string->symbol (string-append 
                   (symbol->string var-prefix)
                   (number->string (counter)))))

(define (counter)
  (begin (set! var-counter
               (add1 var-counter))
         var-counter))

(define (L4-e? e)
  (match e
    [(or (? number?)
         (? var?)
         (? label?)
         `(let ((,(? var?) ,(? L4-e?))) ,(? L4-e?))
         `(if ,(? L4-e?) ,(? L4-e?) ,(? L4-e?))
         `(,(? L4-e?) ...)
         `(new-array ,(? L4-e?) ,(? L4-e?))
         `(new-tuple ,(? L4-e?) ...)
         `(aref ,(? L4-e?) ,(? L4-e?))
         `(aset ,(? L4-e?) ,(? L4-e?) ,(? L4-e?))
         `(alen ,(? L4-e?))
         `(begin ,(? L4-e?) ,(? L4-e?))
         `(print ,(? L4-e?))
         `(make-closure ,(? label?) ,(? L4-e?))
         `(closure-proc ,(? L4-e?))
         `(closure-vars ,(? L4-e?))
         `(,(? biop?) ,(? L4-e?) ,(? L4-e?))
         `(,(? pred?) ,(? L4-e?)))
     #t]
    [_ #f]))


(define (find e k)
  (match e
    [`(begin ,e1 ,e2)
     (find `(let ([,(fresh-var) ,e1]) ,e2) k)]
    [`(let ([,x ,r]) ,b)
     (find r (let-ctxt x b k))]
    [`(if ,c ,t ,e)
     (find c (if-ctxt t e k))]
    [`(,f ,a ...)
     (find f (fun-ctxt a k))]
    [(? val?)
     (fill e k)]))

(define (maybe-let d f)
  (if (val? d)
      (f d)
      (let ([x (fresh-var)])
        `(let ([,x ,d])
           ,(f x)))))

(define (new-var-name var)
  (if (symbol? var)
      (string->symbol (string-append
                       (symbol->string var)
                       "_var_"
                       (number->string (counter))))
      var))


(define (replace-var expr var new-var)
  (cond
    [(equal? var expr) new-var]
    [(pair? expr) (cons (replace-var (car expr) var new-var)
                        (replace-var (cdr expr) var new-var))]
    [else expr]))

(define (fill d k)
  (type-case context k
    [let-ctxt
     (x b k)
     (let* ([new_x (new-var-name x)]
            [new_b (replace-var b x new_x)])
       `(let ([,new_x ,d])
          ,(find new_b k)))]
    [if-ctxt
     (t e k)
     (maybe-let d
                (λ (v)
                  `(if ,v
                       ,(find t k)
                       ,(find e k))))]
    [fun-ctxt
     (a k)
     (if (empty? a)
         (maybe-let d
                    (λ (v)
                      (fill `(,v) k)))
         (maybe-let d
                    (λ (v)
                      (find (first a)
                            (arg-ctxt v
                                      '()
                                      (rest a)
                                      k)))))]
    [arg-ctxt
     (f sub-norm sub-remain k)
     (if (empty? sub-remain)
         (maybe-let d
                    (λ (v)
                      (fill
                       (append `(,f) sub-norm (list v)) k)))
         (maybe-let d
                    (λ (v)
                      (find (first sub-remain)
                            (arg-ctxt f
                                      (append sub-norm `(,v))
                                      (rest sub-remain)
                                      k)))))]
    [no-ctxt () d]))

(define (norm e)
  (find e (no-ctxt)))

(define (compile-L4 prog)
    (cons (norm (first prog))
          (map (λ (func)
                 `(,(first func) 
                   ,(second func) 
                   ,(norm (third func))))
               (rest prog))))