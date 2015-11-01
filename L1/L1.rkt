#lang racket

(define (read-from-file filename)
  (call-with-input-file filename read))

(define (print-line string)
  (begin
    (display string)
    (newline)))

(define (a? expr)
  (or (sx? expr) 
      (if (eq? (member expr (list 'rdi 'rsi 'rdx 'r8 'r9))
               #f) #f #t)))

(define (w? expr)
  (or (a? expr) 
      (if (eq? (member expr (list 'rax 'rbx 'rbp 'r10 'r11 'r12 'r13 'r14 'r15))
               #f) #f #t)))

(define (x? expr)
  (or (w? expr) (eq? 'rsp expr)))

(define (s? expr)
  (or (x? expr) (number? expr) (l1-label? expr)))

(define (t? expr)
  (or (x? expr) (number? expr)))

(define (u? expr)
  (or (x? expr) (l1-label? expr)))

(define (sx? expr)
  (eq? 'rcx expr))

(define (l1-label? expr)
  (if (symbol? expr)
      (match (symbol->string expr)
        [(regexp #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$") #t]
        [_ #f])
      #f))

(define (parse-label lbl)
  (substring (symbol->string lbl) 1))

(define (aop? expr)
  (cond 
    [(string=? (symbol->string expr) "+=") #t]
    [(string=? (symbol->string expr) "-=") #t]
    [(string=? (symbol->string expr) "*=") #t]
    [(string=? (symbol->string expr) "&=") #t]
    [else #f]))

(define (parse-aop aop)
  (cond 
    [(string=? (symbol->string aop) "+=") "addq"]
    [(string=? (symbol->string aop) "-=") "subq"]
    [(string=? (symbol->string aop) "*=") "imulq"]
    [(string=? (symbol->string aop) "&=") "andq"]
    [else #f]))

(define (sop? expr)
  (cond 
    [(string=? (symbol->string expr) "<<=") #t]
    [(string=? (symbol->string expr) ">>=") #t]
    [else #f]))

(define (parse-sop sop)
  (cond 
    [(string=? (symbol->string sop) "<<=") "salq"]
    [(string=? (symbol->string sop) ">>=") "sarq"]
    [else #f]))

(define (cmp? expr)
  (cond 
    [(string=? (symbol->string expr) "<") #t]
    [(string=? (symbol->string expr) "<=") #t]
    [(string=? (symbol->string expr) "=") #t]
    [else #f]))

(define (parse-cmp cmp)
  (cond 
    [(string=? (symbol->string cmp) "<") "setl"]
    [(string=? (symbol->string cmp) "<=") "setle"]
    [(string=? (symbol->string cmp) "=") "sete"]
    [else #f]))

(define (parse-cmp-flip cmp)
  (cond 
    [(string=? (symbol->string cmp) "<") "setg"]
    [(string=? (symbol->string cmp) "<=") "setge"]
    [(string=? (symbol->string cmp) "=") "sete"]
    [else #f]))

(define (parse-cjump cmp)
  (cond 
    [(string=? (symbol->string cmp) "<") "jl"]
    [(string=? (symbol->string cmp) "<=") "jle"]
    [(string=? (symbol->string cmp) "=") "je"]
    [else #f]))

(define (parse-cjump-flip cmp)
  (cond 
    [(string=? (symbol->string cmp) "<") "jg"]
    [(string=? (symbol->string cmp) "<=") "jge"]
    [(string=? (symbol->string cmp) "=") "je"]
    [else #f]))


(define (lower8b-reg reg)
  (cond
    [(eq? 'rax reg) "al"]
    [(eq? 'rcx reg) "cl"]
    [(eq? 'rbx reg) "bl"]
    [(eq? 'rdx reg) "dl"]
    [(eq? 'rdi reg) "dil"]
    [(eq? 'rsi reg) "sil"]
    [(eq? 'rbp reg) "bpl"]
    [(eq? 'r8 reg) "r8b"]
    [(eq? 'r9 reg) "r9b"]
    [(eq? 'r10 reg) "r10b"]
    [(eq? 'r11 reg) "r11b"]
    [(eq? 'r12 reg) "r12b"]
    [(eq? 'r13 reg) "r13b"]
    [(eq? 'r14 reg) "r14b"]
    [(eq? 'r15 reg) "r15b"]))

(define (cmp-two-nums lnum rnum cmp)
  (cond
    [(eq? '<= cmp) (if (<= lnum rnum) "1" "0")]
    [(eq? '<  cmp) (if (<  lnum rnum) "1" "0")]
    [(eq? '=  cmp) (if (=  lnum rnum) "1" "0")]))

(define (compile-L1 expr)
  (match expr
    ;;(register <- label)
    [(list (? symbol? dst) '<- (? l1-label? lbl))
     (write-file (string-append "movq "
                                (string-append "$_" (parse-label lbl))
                                ", "
                                (string-append "%" (symbol->string dst))))]
    ;;(register <- number)
    [(list (? symbol? dst) '<- (? number? num))
     (write-file (string-append "movq " 
                                (string-append "$" (number->string num))
                                ", "
                                (string-append "%" (symbol->string dst))))]
    ;;(register <- register)
    [(list (? symbol? dst) '<- (? symbol? reg))
     (write-file (string-append "movq "
                                (string-append "%" (symbol->string reg))
                                ", "
                                (string-append "%" (symbol->string dst))))]
    ;;(mem <- lbl)
    [(list (list 'mem (? symbol? dst) (? number? num)) '<- (? l1-label? lbl))
     (write-file (string-append "movq "
                                (string-append "$_" (parse-label lbl))
                                ", "
                                (string-append (number->string num) "(%" (symbol->string dst) ")")))]
    ;;(mem <- register) qufen reg
    [(list (list 'mem (? symbol? dst) (? number? num)) '<- (? symbol? reg))
     (write-file (string-append "movq "
                                (string-append "%" (symbol->string reg))
                                ", "
                                (string-append (number->string num) "(%" (symbol->string dst) ")")))]
    ;;(mem <- num) qufen reg
    [(list (list 'mem (? symbol? dst) (? number? num)) '<- (? number? num1))
     (write-file (string-append "movq "
                                (string-append "$" (number->string num1))
                                ", "
                                (string-append (number->string num) "(%" (symbol->string dst) ")")))]
    ;;(register <- mem) qufen reg
    [(list (? symbol? reg) '<- (list 'mem (? symbol? dst) (? number? num)))
     (write-file (string-append "movq "
                                (string-append (number->string num) "(%" (symbol->string dst) ")")
                                ", "
                                (string-append "%" (symbol->string reg))))]
    ;;aop (dst aop= reg)
    [(list (? symbol? dst) (? aop? aop) (? symbol? reg))
     (write-file (string-append (parse-aop aop)
                                " "
                                (string-append "%" (symbol->string reg))
                                ", "
                                (string-append "%" (symbol->string dst))))]
    ;;aop (dst aop= num)
    [(list (? symbol? dst) (? aop? aop) (? number? num))
     (write-file (string-append (parse-aop aop)
                                " "
                                (string-append "$" (number->string num))
                                ", "
                                (string-append "%" (symbol->string dst))))]
    ;;sop (dst sop= reg|num) ****count reg
    [(list (? symbol? dst) (? sop? sop) (? sx? reg))
     (write-file (string-append (parse-sop sop)
                                " "
                                (string-append "%cl")
                                ", "
                                (string-append "%" (symbol->string dst))))]
    [(list (? symbol? dst) (? sop? sop) (? number? num))
     (write-file (string-append (parse-sop sop)
                                " "
                                (string-append "$" (number->string num))
                                ", "
                                (string-append "%" (symbol->string dst))))]
    
    ;;cmp (dst <- lhs cmp rhs)
    [(list (? symbol? dst) '<- (? symbol? lhs) (? cmp? cmp) (? symbol? rhs))
     (write-file (string-append "cmpq "
                                (string-append "%" (symbol->string rhs))
                                ", "
                                (string-append "%" (symbol->string lhs))))
     (write-file (string-append (parse-cmp cmp)
                                " "
                                (string-append "%" (lower8b-reg dst))))
     (write-file (string-append "movzbq "
                                (string-append "%" (lower8b-reg dst))
                                ", "
                                (string-append "%" (symbol->string dst))))]
    
    ;;cmp (dst <- lhs cmp num)
    [(list (? symbol? dst) '<- (? symbol? lhs) (? cmp? cmp) (? number? num))
     (write-file (string-append "cmpq "
                                (string-append "$" (number->string num))
                                ", "
                                (string-append "%" (symbol->string lhs))))
     (write-file (string-append (parse-cmp cmp)
                                " "
                                (string-append "%" (lower8b-reg dst))))
     (write-file (string-append "movzbq "
                                (string-append "%" (lower8b-reg dst))
                                ", "
                                (string-append "%" (symbol->string dst))))]
    
    ;;cmp (dst <- num cmp rhs)
    [(list (? symbol? dst) '<- (? number? num) (? cmp? cmp) (? symbol? rhs))
     (write-file (string-append "cmpq "
                                (string-append "$" (number->string num))
                                ", "
                                (string-append "%" (symbol->string rhs))))
     (write-file (string-append (parse-cmp-flip cmp)
                                " "
                                (string-append "%" (lower8b-reg dst))))
     (write-file (string-append "movzbq "
                                (string-append "%" (lower8b-reg dst))
                                ", "
                                (string-append "%" (symbol->string dst))))]
    
    ;;cmp (dst <- lnum cmp rnum)
    [(list (? symbol? dst) '<- (? number? lnum) (? cmp? cmp) (? number? rnum))
     (write-file (string-append "movq "
                                (string-append "$" (cmp-two-nums lnum rnum cmp))
                                ", "
                                (string-append "%" (symbol->string dst))))]
    
    ;;label
    [(? l1-label? lbl)
     (write-file (string-append "_" (parse-label lbl) ":"))]
    
    ;;goto (goto :lbl)
    [(list 'goto (? l1-label? lbl))
     (write-file (string-append "jmp _" (parse-label lbl)))]
    
    ;;cjump (cjump lhs cmp rhs tlbl flbl)
    [(list 'cjump (? symbol? lhs) (? cmp? cmp) (? symbol? rhs) (? l1-label? tlbl) (? l1-label? flbl))
     (write-file (string-append "cmpq "
                                (string-append "%" (symbol->string rhs))
                                ", "
                                (string-append "%" (symbol->string lhs))))
     (write-file (string-append (parse-cjump cmp)
                                " "
                                "_"
                                (parse-label tlbl)))
     (write-file (string-append "jmp "
                                "_"
                                (parse-label flbl)))]
    
    ;;cjump (cjump lhs cmp num tlbl flbl)
    [(list 'cjump (? symbol? lhs) (? cmp? cmp) (? number? num) (? l1-label? tlbl) (? l1-label? flbl))
     (write-file (string-append "cmpq "
                                (string-append "$" (number->string num))
                                ", "
                                (string-append "%" (symbol->string lhs))))
     (write-file (string-append (parse-cjump cmp)
                                " "
                                "_"
                                (parse-label tlbl)))
     (write-file (string-append "jmp "
                                "_"
                                (parse-label flbl)))]
    
    ;;cjump (cjump num cmp lhs tlbl flbl)
    [(list 'cjump (? number? num) (? cmp? cmp) (? symbol? rhs) (? l1-label? tlbl) (? l1-label? flbl))
     (write-file (string-append "cmpq "
                                (string-append "$" (number->string num))
                                ", "
                                (string-append "%" (symbol->string rhs))))
     (write-file (string-append (parse-cjump-flip cmp)
                                " "
                                "_"
                                (parse-label tlbl)))
     (write-file (string-append "jmp "
                                "_"
                                (parse-label flbl)))]
    
    ;;cjump (cjump lnum cmp rnum tlbl flbl)
    [(list 'cjump (? number? lnum) (? cmp? cmp) (? number? rnum) (? l1-label? tlbl) (? l1-label? flbl))
     (write-file (string-append "jmp "
                                "_"
                                (if (eq? (cmp-two-nums lnum rnum cmp) "1")
                                    (parse-label tlbl)
                                    (parse-label flbl))))]
    ;;function header
    [(list (? l1-label? lbl) (? number? args) (? number? lcl)) 1]
    ;;call
    [(list 'call (? l1-label? flbl) (? number? regnum))
     (write-file (string-append "subq "
                                (string-append "$" (number->string (if (> regnum 6)
                                                                       (+ (* (- regnum 6) 8) 8)
                                                                       8)))
                                ", %rsp"))
     (write-file (string-append "jmp _" (parse-label flbl)))]
    ;;call reg
    [(list 'call (? x? x) (? number? regnum))
     (write-file (string-append "subq "
                                (string-append "$" (number->string (if (> regnum 6)
                                                                       (+ (* (- regnum 6) 8) 8)
                                                                       8)))
                                ", %rsp"))
     (write-file (string-append "jmp %" (symbol->string x)))]
    ;;tail-call
    [(list 'tail-call (? l1-label? flbl) (? number? regnum))
     (write-file (string-append "jmp _" (parse-label flbl)))]
    ;;tail-call reg
    [(list 'tail-call (? x? x) (? number? regnum))
     (write-file (string-append "jmp _" (symbol->string x)))]
    
    ;;return
    [(list 'return) (write-file "ret")]
    ;;array-error
    [(list 'call 'array-error (? number? regnum))
     (write-file "call array_error")]
    ;;print
    [(list 'call 'print (? number? regnum))
     (write-file "call print")]
    ;;allocate
    [(list 'call 'allocate (? number? regnum))
     (write-file "call allocate")]                            
    ))

(define (compile-function function)
  (write-file (string-append "_" (parse-label (car function)) ":"))
  (write-file (string-append "subq $"
                             (string-append (number->string (* 8 (third function))))
                             ", %rsp")) 
  (map (lambda (instrs)
         (match instrs
           [(list 'return)
            (begin
              (write-file (string-append "addq $"
                                         (string-append (number->string
                                                         (if (> (second function) 6)
                                                             (+ (* 8 (third function)) (* (- (second function) 6) 8))
                                                             (* (third function) 8)))
                                                        ", %rsp")))
              (compile-L1 instrs))]
           [(list 'tail-call (? l1-label? flbl) (? number? regnum))
            (begin
              (write-file (string-append "addq $"
                                         (string-append (number->string
                                                         (if (> (second function) 6)
                                                             (+ (* 8 (third function)) (* (- (second function) 6) 8))
                                                             (* (third function) 8)))
                                                        ", %rsp")))
              (compile-L1 instrs))]
           [else (compile-L1 instrs)]))
       (cdddr function)))

(define (compile-program prog)
  (write-file ".text")
  (write-file ".globl go")
  (write-file "go:")
  (write-file "pushq %rbx")
  (write-file "pushq %rbp")
  (write-file "pushq %r12")
  (write-file "pushq %r13")
  (write-file "pushq %r14")
  (write-file "pushq %r15")
  (write-file (string-append "call "
                             (string-append "_" (parse-label (car prog)))))
  (write-file "popq %r15")
  (write-file "popq %r14")
  (write-file "popq %r13")
  (write-file "popq %r12")
  (write-file "popq %rbp")
  (write-file "popq %rbx")
  (write-file "retq")
  (map (lambda (function)
         (compile-function function))
       (rest prog)))

(define (write-file x)
  (with-output-to-file "prog.S" #:exists 'append (lambda () (print-line x))))

(when (file-exists? "prog.S")
  (delete-file "prog.S"))
(define in (read-from-file "filename.in"))
;(require racket/cmdline)
;(define filename
;  (command-line
;   #:args (filename) filename))

;(compile-program (read-from-file filename))
(compile-program (read-from-file (symbol->string in)))
