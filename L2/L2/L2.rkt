#lang racket
(require racket/set)

(require "graph.rkt")
(require "liveness.rkt")
(require "spill.rkt")

(define registers
  (set 'r10 'r11 'r12 'r13 'r14 'r15 'r8 'r9 'rax 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi))

(define (register? expr)
  (set-member? registers expr))

(define (var? expr)
  (if (and (symbol? expr) (not (register? expr)))
      (match (symbol->string expr)
        [(regexp #rx"^[a-zA-Z_][a-zA-Z_0-9-]*$") #t]
        [_ #f])
      #f))

(define spill_prefix 's_)

(define (replace-var-to-reg function var reg)
  (map (lambda (instr)
         (if (list? instr)
             (map (lambda (elem)
                    (if (list? elem)
                        (map (lambda (sub-elem)
                               (if (equal? sub-elem var)
                                   reg
                                   sub-elem))
                             elem)
                        (if (equal? elem var)
                            reg
                            elem)))
                  instr)
             instr))
       function))

(define (update-function function colored-vars)
  (if (empty? colored-vars)
      function
      (let ([var (first (first colored-vars))]
            [reg (second (first colored-vars))])
        (update-function (replace-var-to-reg function var reg) (rest colored-vars)))))

(define (update-stack-arg instruction spill-num)
  (match instruction
    [`(,a <- (stack-arg ,n))
     `(,a <- (mem rsp ,(+ n (* 8 spill-num))))]
    [_ instruction]))

#|(define (count-temp-num function)
  (apply + 
         (map (lambda (instruction)
                (match instruction
                  [`((mem rsp ,n) <- ,a) 1]
                  [`(,a <- (mem rsp ,n)) 1]
                  [_ 0]))
              function)))

(define (update-stack-arg-function function)
  (let* ([new-function 
         (map (lambda (instruction)
                (update-stack-arg instruction (third function))) function)]
         [count (count-temp-num new-function)])
    (list* (first new-function)
           (second new-function)
           count
           (cdddr new-function))))|#

(define (compile-function-helper function variables)
  (let ([colored-vars (generate-colored-graph (cdddr function))])
    (if (equal? colored-vars #f)
        (if (empty? variables)
            (display (format "could not register allocate ~a" (first function)))
            (compile-function-helper (spill-function function 
                                                     (first variables)
                                                     spill_prefix)
                                     (rest variables)))
        (update-function function colored-vars)
        )))

(define (compile-function function)
  (let* ([instructions (cdddr function)]
         [vars (function-vars (kill-list instructions) 
                              (in-list instructions) 
                              (out-list instructions))]
         [new-function (compile-function-helper function vars)]
         [spill-num (third new-function)])
    ; spill before color
    (map (lambda (instruction)
                 (update-stack-arg instruction spill-num))
         new-function)))

(define (compile-L2 prog)
  (display (append (list (first prog))
                   (map (lambda (function) (compile-function function))
                        (rest prog)))))

(define in (call-with-input-file "filename.in" read))

(define (read-from-file filename)
  (call-with-input-file filename read))

(compile-L2 (read-from-file in))