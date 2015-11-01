#lang racket
(require racket/set)

;; 
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

(define (u? expr)
  (or (x? expr) (label? expr)))

(define (op? expr)
  (cond
    [(symbol=? expr '+=) #t]
    [(symbol=? expr '-=) #t]
    [(symbol=? expr '*=) #t]
    [(symbol=? expr '&=) #t]
    [(symbol=? expr '<<=) #t]
    [(symbol=? expr '>>=) #t]
    [else #f]))

(define (cmp? expr)
  (cond
    [(symbol=? expr '<=) #t]
    [(symbol=? expr '=) #t]
    [(symbol=? expr '<) #t]
    [else #f]))


;; 
(define callee-save
  (set 'r12 'r13 'r14 'r15 'rbp 'rbx))

(define caller-save
  (set 'r10 'r11 'r8 'r9 'rax 'rcx 'rdi 'rdx 'rsi))

(define args
  (set 'rdi 'rsi 'rdx 'rcx 'r8 'r9))

(define result
  (set 'rax))

(define (genable? expr)
  (and (not (number? expr)) (not (label? expr)) (not (equal? expr 'rsp))))

(define (gen-set expr)
  (if (genable? expr)
      (set expr)
      (set)))

(define (gen-args num)
  (cond
    [(= num 0) (set)]
    [(= num 1) (set 'rdi)]
    [(= num 2) (set 'rdi 'rsi)]
    [(= num 3) (set 'rdi 'rsi 'rdx)]
    [(= num 4) (set 'rdi 'rsi 'rdx 'rcx)]
    [(= num 5) (set 'rdi 'rsi 'rdx 'rcx 'r8)]
    [else args]))


;;
(define (gen-kill-instruction expr)
  (match expr 
    ;; w <- s or w <- (mem x n8)
    [(or (list (? w? dst) '<- (? s? src))
         (list (? w? dst) '<- (list 'mem (? x? src) (? number?))))
     (cons (gen-set src) (gen-set dst))]
    
    ;; (mem x n8) <- s
    [(list (list 'mem (? x? dst) (? number?)) '<- (? s? src))
     (cons (set-union (gen-set dst) (gen-set src)) (set))]
    
    ;; w aop t or w sop sx/num 
    [(list (? w? dst) (? op? op) (? t? src)) ;;FIXME
     (cons (set-union (gen-set src) (gen-set dst)) (gen-set dst))]
    
    ;; w <- t cmp t
    [(list (? w? dst) '<- (? t? src1) (? cmp? cmp) (? t? src2))
     (cons (set-union (gen-set src1) (gen-set src2)) (gen-set dst))]
  
    ;; label or goto label
    [(or (? label? lbl) 
         (list 'goto (? label? lbl)))
     (cons (set) (set))]
    
    ;; cjump t cmp t lbl1 lbl2
    [(list 'cjump (? t? src1) (? cmp? cmp) (? t? src2) (? label? lbl1) (? label? lbl2))
     (cons (set-union (gen-set src1) (gen-set src2)) (set))]
    
    ;;call sys-call num  ;;FIXME  sequence with call u nat
    [(or (list 'call 'print (? number? num))
         (list 'call 'allocate (? number? num))
         (list 'call 'array-error (? number? num)))
     (cons (gen-args num) (set-union caller-save result))]
    
    ;; call u nat
    [(list 'call (? u? src) (? number? num))
     (cons (set-union (gen-set src) (gen-args num)) (set-union caller-save result))]
    
    ;; tail-call u nat0-6
    [(list 'tail-call (? u? src) (? number? num))
     (cons (set-union (gen-set src) (gen-args num) callee-save) (set))]
    
    ;; return 
    [(list 'return)
     (cons (set-union result callee-save) (set))]
    
    ;; w <- (stack-arg n8)
    [(list (? w? dst) '<- (list 'stack-arg (? number? num)))
     (cons (set) (gen-set dst))]))

(define (gen-kill-function list-instr)
  (map (lambda (instr)
         (gen-kill-instruction instr))
       list-instr))
  

;; Successor
(define (hash-table-succ list-instr list-index)
  (make-hash (filter (lambda (index-instr)
                       (label? (car index-instr)))
                     (map (lambda (instr index) (cons instr index))
                          list-instr list-index))))


(define (succ-instruction expr index hash-table-label)
  (match expr
    ;; No Successor (return) (tail-call)
    [(or '(return) 
         (list 'tail-call (? u?) (? number?))
         (list 'call 'array-error (? number?)))
     '()]
    ;; One Successor (goto+ label)
    [(list 'goto (? label? lbl))
     (list (hash-ref hash-table-label lbl))]
    ;; Two Successor (cjump)
    [(list 'cjump (? t?) (? cmp?) (? t?) (? label? lbl1) (? label? lbl2))
     (list (hash-ref hash-table-label lbl1)
           (hash-ref hash-table-label lbl2))]
    [_ (list (+ 1 index))]))

(define (succ-function list-instr)
  (let* ([list-index (build-list (length list-instr) (lambda (x) x))]
         [hash-table-label (hash-table-succ list-instr list-index)])
    (map (lambda (instr index) (succ-instruction instr index hash-table-label))
         list-instr list-index)))

;; In 
(define (in-instruction gen-kill-instr out-instr)
  (set-union (car gen-kill-instr) (set-subtract (list->set out-instr) (cdr gen-kill-instr))))

(define (in-function list-gen-kill-instr list-out-instr)
  (map (lambda (gen-kill-instr out-instr)
         (set->list (in-instruction gen-kill-instr out-instr)))
       list-gen-kill-instr list-out-instr))

;; Out
(define (out-instruction list-in-instr succ-index)
  (if (or (empty? succ-index) 
          (equal? (car succ-index) (length list-in-instr)))
      (set)
      (list->set
       (append* (map (lambda (ind) (list-ref list-in-instr ind))
                     succ-index)))))

(define (out-function list-in-instr list-succ-index)
  (map (lambda (succ-index)
         (set->list (out-instruction list-in-instr succ-index)))
       list-succ-index))

;; Top Level
(define (liveness-function list-gen-kill list-succ list-in-temp list-out-temp)
  (let* ([list-in (in-function list-gen-kill list-out-temp)]
         [list-out (out-function list-in list-succ)])
    (if (and (equal? list-in-temp list-in) (equal? list-out-temp list-out))
        (format-output list-in list-out)
        (liveness-function list-gen-kill list-succ list-in list-out))))

(define (format-output list-in list-out)
  (list (append '(in)
              (map (lambda (lst) (sort-list lst))
                   list-in))
        (append '(out) 
              (map (lambda (lst) (sort-list lst))
                   list-out))))
(define (kill-list list-instr)
  (map (lambda (instr) (cdr instr))
       (gen-kill-function list-instr)))
(provide kill-list)
         
(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (sort-list lst)
  (sort lst symbol<?))
(provide sort-list)

(define (liveness-program-helper list-instr)
  (let* ([list-gen-kill (gen-kill-function list-instr)]
         [list-succ (succ-function list-instr)]
         [list-in-temp (build-list (length list-instr) (lambda (x) '()))]
         [list-out-temp (build-list (length list-instr) (lambda (x) '()))])
    (liveness-function list-gen-kill list-succ list-in-temp list-out-temp)))
(provide liveness-program-helper)

(define (liveness-program list-instr)
  (display (liveness-program-helper list-instr)))

(define (in-list list-instr)
  (map (lambda (instr) (list->set instr))
       (cdr (car (liveness-program-helper list-instr)))))
(provide in-list)

(define (out-list list-instr)
  (map (lambda (instr) (list->set instr))
       (cdr (cadr (liveness-program-helper list-instr)))))
(provide out-list)
   
     
     
     
     
     
     
     