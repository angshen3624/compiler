#lang racket

(define (a? expr)
  (or (sx? expr) 
      (if (eq? (member expr (list 'rdi 'rdx 'rsi 'r8 'r9))
               #f) #f #t)))

(define (w? expr)
  (or (a? expr) 
      (if (eq? (member expr (list 'rax 'rbx 'rbp 'r10 'r11 'r12 'r13 'r14 'r15))
               #f) #f #t)))

(define (x? expr)
  (or (w? expr) (eq? 'rsp expr)))

(define (s? expr)
  (or (x? expr) (number? expr) (label? expr)))

(define (t? expr)
  (or (x? expr) (number? expr)))

(define (u? expr)
  (or (x? expr) (label? expr)))

(define (sx? expr)
  (or (eq? 'rcx expr) (var? expr)))

(define (label? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$") #t]
        [_ #f])
      #f))

(define (var? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^[a-zA-Z_][a-zA-Z_0-9-]*$") #t]
        [_ #f])
      #f))

(define (aop? expr)
  (cond 
    [(string=? (symbol->string expr) "+=") #t]
    [(string=? (symbol->string expr) "-=") #t]
    [(string=? (symbol->string expr) "*=") #t]
    [(string=? (symbol->string expr) "&=") #t]
    [else #f]))

(define (sop? expr)
  (cond 
    [(string=? (symbol->string expr) "<<=") #t]
    [(string=? (symbol->string expr) ">>=") #t]
    [else #f]))

(define (cmp? expr)
  (cond 
    [(string=? (symbol->string expr) "<") #t]
    [(string=? (symbol->string expr) "<=") #t]
    [(string=? (symbol->string expr) "=") #t]
    [else #f]))

(define prefix-num -1)

;;(define offset 0)  ;;FIXME

(define (prefix-inc prefix) 
  (set! prefix-num (+ prefix-num 1))
  (string->symbol (string-append 
                   (symbol->string prefix) 
                   (number->string prefix-num))))
(define (replace-var src var temp-var)
  (if (equal? src var)
      temp-var
      src))


(define (spill-instruction expr var prefix offset)
  (match expr
    ;;(w <- s)
    [(list (? w? dst) '<- (? s? src))
     (cond
       ;; x <- x
       [(and (equal? dst var) (equal? src var)) '()]
       ;; x at left
       [(equal? dst var)
        (list (list (list 'mem 'rsp offset) '<- src))]
       ;; x at right
       [(equal? src var)
        (list (list dst '<- (list 'mem 'rsp offset)))]
       [else (list expr)])]
    
    ;; w <- (mem x n8)
    [(list (? w? dst) '<- (list 'mem (? x? x) (? number? num)))
     (cond
       ;; both sides   x <- (mem x n8) 
       [(and (equal? dst var) (equal? x var))
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var
                      '<- 
                      (list 'mem 'rsp offset))
                (list  temp-var '<- (list 'mem temp-var num))
                (list (list 'mem 'rsp offset) '<- temp-var)))]
       ;; x at left
       [(equal? dst var)
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var
                      '<- 
                      (list 'mem x num))
                (list (list 'mem 'rsp offset) 
                      '<-
                      temp-var)))]
       
       ;;(list (list 'mem 'rsp offset) '<- (list 'mem x num))]
       ;; x at right
       [(equal? x var)
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<- 
                      (list 'mem 'rsp offset))
                (list dst '<- (list 'mem temp-var num))))]
       [else (list expr)])]
    
    ;;(mem x n8) <- s
    [(list (list 'mem (? x? dst) (? number? num)) '<- (? s? src))
     (cond 
       ;;both sides
       [(and (equal? dst var) (equal? src var))
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<- 
                      (list 'mem 'rsp offset))
                (list (list 'mem temp-var num)
                      '<-
                      temp-var)))]
       ;;at left
       [(equal? dst var)
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var
                      '<-
                      (list 'mem 'rsp offset))
                (list (list 'mem temp-var num)
                      '<-
                      src)))]
       ;;at right
       [(equal? src var)
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list (list 'mem dst num)
                      '<-
                      temp-var)))]
       [else (list expr)])]
    
    ;; w aop t
    [(list (? w? dst) (? aop? aop) (? t? src))
     (cond
       ;;both sides
       [(and (equal? dst var) (equal? src var))
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list temp-var aop temp-var)
                (list (list 'mem 'rsp offset) '<- temp-var)))]
       
       ;; at left
       [(and (equal? dst var))
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list temp-var aop src)
                (list (list 'mem 'rsp offset) '<- temp-var)))]
       
       ;; at right
       [(and (equal? src var))
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list dst aop temp-var)))]
       [else (list expr)])]
    
    ;; w sop t
    [(or (list (? w? dst) (? sop? sop) (? sx? src)) 
         (list (? w? dst) (? sop? sop) (? number? src)))
     (cond
       ;;both sides
       [(and (equal? dst var) (equal? src var))
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list temp-var sop temp-var)
                (list (list 'mem 'rsp offset) '<- temp-var)))]
       
       ;; at left
       [(and (equal? dst var))
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list temp-var sop src)
                (list (list 'mem 'rsp offset) '<- temp-var)))]
       
       ;; at right
       [(and (equal? src var))
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list dst sop temp-var)))]
       [else (list expr)])]
    
    ;; dst <- src1 cmp src2
    [(list (? w? dst) '<- (? t? src1) (? cmp? cmp) (? t? src2))
     (cond 
       ;; both sides
       [(and (equal? dst var) (or (equal? src1 var) (equal? src2 var)))
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list temp-var '<- (replace-var src1 var temp-var) 
                      cmp 
                      (replace-var src2 var temp-var))
                (list (list 'mem 'rsp offset)
                            '<- 
                            temp-var)))]
       
       ;; left side 
       [(equal? dst var)
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var '<- (replace-var src1 var temp-var) 
                      cmp 
                      (replace-var src2 var temp-var))
                (list (list 'mem 'rsp offset)
                            '<- 
                            temp-var)))]
       
       ;; right side
       [(or (equal? src1 var) (equal? src2 var))
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list dst '<- (replace-var src1 var temp-var) 
                      cmp
                      (replace-var src2 var temp-var))))]
       [else (list expr)])]
    
    ;; cjump t cmp t lbl1 lbl2
    [(list 'cjump (? t? src1) (? cmp? cmp) (? t? src2) (? label? lbl1) (? label? lbl2))
     (cond 
       ;; 
       [(or (equal? src1 var) (equal? src2 var))
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list 'cjump (replace-var src1 var temp-var) 
                      cmp
                      (replace-var src2 var temp-var)
                      lbl1 lbl2)))]
       [else (list expr)])]
    
    ;; call u nat
    [(list 'call (? u? src) (? number? num))
     (cond
       ;;
       [(equal? src var)
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list 'call 
                      (replace-var src var temp-var)
                      num)))]
       [else (list expr)])]
    
    ;; tail-call u nat0-6
    [(list 'tail-call (? u? src) (? number? num))
     (cond
       ;;
       [(equal? src var)
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var 
                      '<-
                      (list 'mem 'rsp offset))
                (list 'tail-call 
                      (replace-var src var temp-var)
                      num)))]
       [else (list expr)])]
    
    ;; w <- (stack-arg n8)
    [(list (? w? dst) '<- (list 'stack-arg (? number? num)))
     (cond 
       ;;
       [(equal? dst var)
        (let ([temp-var (prefix-inc prefix)])
          (list (list temp-var '<- (list 'stack-arg num))
                (list (list 'mem 'rsp offset)
                      '<-
                      temp-var)))]
       [else (list expr)])]
    [_ (list expr)]))

(define (read-from-file filename)
  (call-with-input-file filename (lambda (port) 
                                   (list (read port) (read port) (read port)))))

(define (spill-function function var prefix)
  (append (list (first function)
                (second function)
                (+ (third function) 1))
          (append* (map 
                    (lambda (instrs) (spill-instruction instrs var prefix (* 8 (third function))))
                    (cdddr function)))))

(provide spill-function)

(define (spill-program filename)
  (let ([prog (read-from-file filename)])
        (display (spill-function (first prog) (second prog) (third prog)))))


;;(define in (call-with-input-file "filename.in" read))
;;(spill-program in)

  