#lang racket

(define dup-set (set))
(define reserved-set (set 'mem 'goto 'cjump 'call 'tail-call
                          'return 'allocate 'array-error))
(define dup-var 'niubi)
(define new-body '())
(define (replace-var sexp find-var repl-var)
  (cond
    [(equal? find-var sexp) repl-var]
    [(pair? sexp) (cons (replace-var (car sexp) find-var repl-var)
                        (replace-var (cdr sexp) find-var repl-var))]
    [else sexp]))


(define syntax-set (set 'new-array 'new-tuple 'aref 'aset 'alen 'print
                            'make-closure 'closure-proc 'closure-vars 
                            'let 'if))


(define (syntax-set? expr)
  (set-member? syntax-set expr))

(define (label? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$") #t]
        [_ #f])
      #f))


(define (var? expr)
  (if (symbol? expr)
      (if (not (syntax-set? expr))
               (match (symbol->string expr)
                 [(regexp #rx"^[a-zA-Z_][a-zA-Z_0-9-]*$") #t]
                 [_ #f])
               #f)
      #f))

(define (encode num)
  (if (number? num)
      (+ 1 (* 2 num))
      num))

(define biop (set '+'- '* '= '< '<= '=))

(define (biop? expr)
  (set-member? biop expr))

(define pred (set 'a? 'number?))

(define (pred? expr)
  (set-member? pred expr))

(define arg-list (list 'rdi 'rsi 'rdx 'rcx 'r8 'r9))

(define var-counter -1)
(define lbl-counter -1)
(define var-prefix 'var_)
(define lbl-prefix ':lbl)

;; 0 for var, 1 for label
(define (gen-symbol num-flag)
  (if (equal? num-flag 1)
      (string->symbol (string-append 
                       (symbol->string lbl-prefix)
                       (number->string (begin (set! lbl-counter
                                                     (add1 lbl-counter))
                                               lbl-counter))))
      (string->symbol (string-append 
                       (symbol->string var-prefix)
                       (number->string (begin (set! var-counter
                                                     (add1 var-counter))
                                               var-counter))))))

                                       

;;
(define (to-L3-instr expr)
  (match expr
    [`(make-closure ,(? label? l) ,v)
     `(new-tuple ,1 ,v)]
    [`(closure-proc ,v)
     `(aref ,v ,(encode 0))]
    [`(closure-vars ,v)
     `(arfe ,v ,(encode 1))]
    [_ expr]))

;; d helper
(define (compile-d-biop op v1 v2 symbol)
  (match op
    ['+
     `((,symbol <- ,(encode v1))
       (,symbol += ,(encode v2))
       (,symbol -= 1))]
    ['-
     `((,symbol <- ,(encode v1))
       (,symbol -= ,(encode v2))
       (,symbol += 1))]
    ['*
     (let ([tmp (gen-symbol 0)])
       `((,tmp <- ,(encode v1))
         (,tmp >>= 1)
         (,symbol <- ,(encode v2))
         (,symbol >>= 1)
         (,symbol *= ,tmp)
         (,symbol *= 2)
         (,symbol += 1)))]
    [else
     `((,symbol <- ,(encode v1) ,op ,(encode v2))
       (,symbol *= 2)
       (,symbol += 1))]))

(define (compile-d-pred num-a v symbol)
  (match num-a
    ['a?
     `((,symbol <- ,(encode v))
       (,symbol &= 1)
       (,symbol *= -2)
       (,symbol += 3))]
    ['number?
     `((,symbol <- ,(encode v))
       (,symbol &= 1)
       (,symbol <<= 1)
       (,symbol += 1))]))

(define (compile-d-aset v1 v2 v3 symbol)
  (let* ([tmp (gen-symbol 0)]
         [lbl1 (gen-symbol 1)]
         [lbl2 (gen-symbol 1)]
         [lbl3 (gen-symbol 1)])
    `((,symbol <- ,(encode v2))
      (,symbol >>= 1)
      (,tmp <- (mem ,v1 0))
      (cjump ,symbol < 0 ,lbl2 ,lbl3)
      ,lbl3
      (cjump ,symbol < ,tmp ,lbl1, lbl2)
      ,lbl2
      (rdi <- ,v1)
      (rsi <- ,(encode v2))
      (call array-error 2)
      ,lbl1
      (,symbol *= 8)
      (,symbol += ,v1)
      ((mem ,symbol 8) <- ,(encode v3))
      (,symbol <- 1))))
         
(define (compile-d-aref v1 v2 symbol)
    (let* ([tmp (gen-symbol 0)]
         [lbl1 (gen-symbol 1)]
         [lbl2 (gen-symbol 1)]
         [lbl3 (gen-symbol 1)])
    `((,symbol <- ,(encode v2))
      (,symbol >>= 1)
      (,tmp <- (mem ,v1 0))
      (cjump ,symbol < 0 ,lbl2 ,lbl3)
      ,lbl3
      (cjump ,symbol < ,tmp ,lbl1, lbl2)
      ,lbl2
      (rdi <- ,v1)
      (rsi <- ,(encode v2))
      (call array-error 2)
      ,lbl1
      (,symbol *= 8)
      (,symbol += ,v1)
      (,symbol <- (mem ,symbol 8)))))

(define (tuple-mem-assigns args len symbol)
  (let ([mem-list '()])
    (for ([i (in-range len)])
      (set! mem-list 
            (append mem-list 
                    `(((mem ,symbol ,(+ (* i 8) 8))
                       <- ,(encode (list-ref args i)))))))
    mem-list))

(define (f-args-assigns args args-num)
  (let ([temp-list '()])
    (for ([i (in-range args-num)])
      (if (<= i 5)
          (set! temp-list 
                (append temp-list 
                        `((,(list-ref arg-list i) 
                           <- ,(encode (list-ref args i))))))
          (set! temp-list
                (append temp-list
                        `(((mem rsp ,(* (- 4 i) 8)) 
                           <- ,(encode (list-ref args i))))))))
      
    temp-list))

(define (lbl-args-assigns args args-num)
  (let ([temp-list '()])
    (for ([i (in-range args-num)])
      (if (<= i 5)
          (set! temp-list 
                (append temp-list 
                        `((,(encode (list-ref args i))
                           <- ,(list-ref arg-list i)))))
          (set! temp-list
                (append temp-list
                       `((,(encode (list-ref args i))
                           <- (stack-arg ,(* (- (- args-num i) 1) 8))))))))
    
    temp-list))

(define (return-call expr return-flag)
  (if (equal? 1 return-flag)
      (append expr (list '(return)))
      expr))

;; d
(define (compile-d expr symbol return-flag)
  (match expr
    [(list (? biop? op) v1 v2)
     (return-call (compile-d-biop op v1 v2 symbol) 
                  return-flag)] 
    
    [(list (? pred? num-a) v)
     (return-call (compile-d-pred num-a v symbol)
                  return-flag)]
    
    [(list 'new-array v1 v2)
     (return-call
      `((rdi <- ,(encode v1))
        (rsi <- ,(encode v2))
        (call allocate 2)
        (,symbol <- rax))
      return-flag)]
    
    [(or (list 'new-tuple args ...) (list 'make-closure args ...))
     (return-call
      (let ([len (length args)])
        (append `((rdi <- ,(encode len))
                  (rsi <- 1)
                  (call allocate 2)
                  (,symbol <- rax))
                (tuple-mem-assigns args len symbol)))
      return-flag)]
             
    [(list 'aref v1 v2)
     (return-call (compile-d-aref v1 v2 symbol)
                  return-flag)]
    
    [(list 'closure-proc v)
     (return-call (compile-d-aref v 0 symbol)
                  return-flag)]
    
    [(list 'closure-vars v)
     (return-call (compile-d-aref v 1 symbol)
                  return-flag)]
    
    [(list 'aset v1 v2 v3)
     (return-call (compile-d-aset v1 v2 v3 symbol)
                  return-flag)]
    
    [(list 'alen v)
     (return-call 
      `((,symbol <- (mem ,v 0))
        (,symbol <<= 1)
        (,symbol += 1)) 
      return-flag)]
    
    [(list 'print v)
     (return-call
      `((rdi <- ,(encode v))
        (call print 1))
        return-flag)]
    
    [(list (? label? lbl) (list args ...) body)  ;FIXME lacking body
     (let ([len (length args)])
       (append (list lbl)
               (list len 0)
               (lbl-args-assigns args len)
               (compile-e body)))]
    
    [(list f args ...)   
     (let* ([len (length args)]
            [lbl (gen-symbol 1)])
       (cond
         [(and (equal? 1 return-flag) 
               (<= len 6))
          (append  (f-args-assigns args len)
                   `((tail-call ,f ,len)))]
         [(and (equal? 0 return-flag))
          (append `(((mem rsp -8) <- ,lbl))
                  (f-args-assigns args len)
                  `((call ,f, len))
                  `(,lbl)
                  `((,symbol <- rax)))]
         [(and (equal? 1 return-flag) (>= len 6))
          (append `(((mem rsp -8) <- ,lbl))
                  (f-args-assigns args len)
                  `((call ,f, len))
                  `(,lbl)
                  `((return)))]))]
   
    [else (return-call 
           `((,symbol <- ,(encode expr))) 
           return-flag)]))

;; e helper
(define (compile-let var expr body)
    (if (set-member? (set-union dup-set reserved-set) var)
      (begin (set! dup-var (gen-symbol 0))
             (set! new-body (replace-var body var dup-var))
             (set! dup-set (set-union dup-set (set dup-var))))
      (begin (set! dup-var var)
             (set! new-body body)))
  (set! dup-set (set-union dup-set (set var)))
  (match expr
    [(or (? number? expr)
         (? symbol? expr)
         (? label? expr))
     (append (list (list dup-var '<- (encode expr))) (compile-e new-body))]
    [else
     (append (compile-d expr dup-var 0) (compile-e new-body))]))

(define (compile-if v e1 e2)
  (let * ([lbl1 (gen-symbol 1)]
          [lbl2 (gen-symbol 1)])
    `((cjump ,(encode v) = 1, lbl1, lbl2)
      ,lbl2
      ,@(compile-e e1)
      ,lbl1
      ,@(compile-e e2))))
  
;; e
(define (compile-e expr)
  (match expr
    [(list 'let (list (list var d)) body)
     (compile-let var d body)]
    [(list 'if v e1 e2)
     (compile-if v e1 e2)]
    [else
     (compile-d expr 'rax 1)]))
         
;; p
(define (compile-L3 prog)
  `(:zhongwendehuajiubuhuichuwenti
    (:zhongwendehuajiubuhuichuwenti
     0 0
     ,@(compile-e (first prog)))
    ,@(map (lambda (func)
             (compile-e func))
           (rest prog))))
          
          
(define in (call-with-input-file "filename.in" read))

(define (read-from-file filename)
  (call-with-input-file filename read))

(pretty-display (compile-L3 (read-from-file in)))
  

(define ll 
  `(
    (let ([x 5])
      (let ([x (- 3 x)])
        (print x)))
    ))

(define lll
  `(
    (let ((a -1))
      (let ((a (- 5 a))) 
        (print a)))))
  
  
  
  
  
  
  
  
  
  
  
  
  
  