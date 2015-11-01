#lang racket
(require racket/set)
(require "liveness.rkt")

(define (sop? expr)
  (cond
    [(symbol=? expr '<<=) #t]
    [(symbol=? expr '>>=) #t]
    [else #f]))

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

(define (get-var expr)
  (set-subtract expr registers))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (remove-if-subset st st2)
  (if (subset? st st2)
      (set-map st (lambda (x) (set-add (set-subtract st2 st) x)))
      (list st2)))

(define (remove-subsets list-of-sets st)
  (foldl (lambda (element result)
           (let ([new-element (remove-if-subset st element)])
                 (append result new-element)))
         (list)
         list-of-sets))

;;constrain instruction
(define (constrain-instruction expr)
  (match expr
    [`(,write ,(? sop? sop) ,(? var? read))
     (set-add (set-subtract registers (set 'rcx)) read)]
    [_ (set)]))

;;interfere instruction
(define (interfere-instruction-at-same-time out)
  (list out))

(define (interfere-instruction-at-same-time-first in out)
  (list in out))

(define (interfere-instruction-kill-out kill out)
  (set-map kill (lambda (elem) (set-add out elem))))

(define (interfere-instruction-general kill out)
  (append (interfere-instruction-at-same-time out)
          (interfere-instruction-kill-out kill out)))

(define (interfere-instruction-general-first kill in out)
  (append (interfere-instruction-at-same-time-first in out)
          (interfere-instruction-kill-out kill out)))

(define (interfere-instruction expr kill out)
  (match expr
    [`(,(? var? write) <- ,(? var? read))
     (remove-subsets 
      (interfere-instruction-general kill out)
      (set write read))]
    [`(,(? var? write) <- ,(? register? read))
     (remove-subsets 
      (interfere-instruction-general kill out)
      (set write read))]
    [_ (interfere-instruction-general kill out)]))

(define (interfere-instruction-first expr kill in out)
  (match expr
    [`(,(? var? write) <- ,(? var? read))
     (remove-subsets
      (interfere-instruction-general-first kill in out)
      (set write read))]
    [`(,(? var? write) <- ,(? register? read))
     (remove-subsets
      (interfere-instruction-general-first kill in out)
      (set write read))]
    [_ (interfere-instruction-general-first kill in out)]))

(define (instruction-vars kill in out)
  (filter var? (set->list (set-union kill in out))))

(define (function-vars kills ins outs)
  (set->list 
   (list->set 
    (foldl (lambda (kill in out result)
           (append result (instruction-vars kill in out)))
         '()
         kills
         ins
         outs))))


;;
(define initial-graph
  (make-immutable-hash
   (set-map registers
            (lambda (x) (cons x (set-remove registers x))))))

(define (generate-initial-graph graph kills ins outs)
  (foldl (lambda (var result)
           (if (hash-has-key? result var)
               result
               (hash-set result var (set))))
         graph
         (function-vars kills ins outs)));;taichangle list->set

(define (add-interference-to-graph graph instr)
  (foldl (lambda (elem result) 
           (hash-set result elem 
                     (set-union 
                      (hash-ref result elem) 
                      (set-remove instr elem))))
         graph
         (set->list instr)))

(define (generate-graph-by-instruction graph interfere-instruction);;interfere-instruction
  (foldl (lambda (instr result) (add-interference-to-graph result instr))
         graph
         interfere-instruction))

(define (generate-graph-by-function instructions kills ins outs)
  (foldl
   (lambda (instruction kill out result) 
     (generate-graph-by-instruction
      (add-interference-to-graph
       result
       (constrain-instruction instruction))
      (interfere-instruction instruction kill out)))
   (generate-graph-by-instruction 
    (add-interference-to-graph 
     (generate-initial-graph initial-graph kills ins outs)
     (constrain-instruction (first instructions)))
    (interfere-instruction-first (first instructions) (first kills) (first ins) (first outs)))
   (cdr instructions)
   (cdr kills)
   (cdr outs)))

;;
(define variable-table
  (make-immutable-hash))
(define (init-variable-table vars);;vars == (function-vars kills ins outs)
  (foldl (lambda (key result) 
           (hash-set result key (set)))
         variable-table
         vars))

(define (sorted-variables vars);;FIX ME heuristic
  (sort (set->list (list->set vars))
        symbol<?))

(define (get-var-color variable var-table);;
  (hash-ref var-table variable))


;;var-neighbors interfere-graph hash-ref return a set of neighbors
(define (color-var var-neighbors);;varibale = for var in vars var 
  (let ([available-colors                 ;; var-neighbors consider varibales that have been colored
         (sort (set->list (set-subtract registers var-neighbors))
               symbol<?)])
    (if (empty? available-colors)
        #f
        (first available-colors))))

(define (update-var-neighbors var-neighbors var-table);;var-table = init-variable-table
  (foldl
   (lambda (elem result)
     (if (register? elem)
         (set-add result elem)
         (set-union result (get-var-color elem var-table))))
   (set)
   (set->list var-neighbors)))

(define (add-color-to-variable-table variable var-neighbors var-table);;var-neighbors from interference graph
  (let ([var-color (color-var (update-var-neighbors var-neighbors var-table))])
    (if (equal? var-color #f)
        var-table
        (hash-set var-table variable (set var-color)))))

(define (final-variable-table vars interference-graph)
  (let ([var-table (init-variable-table vars)])
    (foldl (lambda (variable result)
               (add-color-to-variable-table variable 
                                          (hash-ref interference-graph variable)
                                          result))
           var-table
           (sorted-variables vars))))

;;

(define (list-by-first<? l1 l2)
  (string<? (symbol->string (first l1)) (symbol->string (first l2))))

(define (has-empty-set? graph)
  (foldl
   (lambda (elem result) (or result (if (equal? (hash-ref graph elem) (set))
                                        #t
                                        #f)))
   #f
   (hash-keys graph)))
   

(define (format-interference-graph graph)
  (sort
   (hash-map
    graph
    (lambda (key value) (list* key (sort (set->list value) symbol<?))))
   list-by-first<?))

(define (format-colored-graph graph)
  (if (has-empty-set? graph)
      #f
      (sort 
        (hash-map
         graph
         (lambda (key value) 
           (list* key (sort (set->list value) symbol<?))))
       list-by-first<?)))
 
(define (generate-graph instructions)
  (begin
    (display (format-interference-graph 
              (generate-graph-by-function 
               instructions
               (kill-list instructions)
               (in-list instructions)
               (out-list instructions))))
    (display (format-colored-graph
              (final-variable-table 
               (set->list (list->set (function-vars (kill-list instructions) 
                                                    (in-list instructions) 
                                                    (out-list instructions))))
               (generate-graph-by-function 
                instructions
                (kill-list instructions)
                (in-list instructions)
                (out-list instructions)))))))

(define in (call-with-input-file "filename.in" read))

(define (read-from-file filename)
  (call-with-input-file filename read))

(generate-graph (cdddr (read-from-file in)))

(define instructions (cdddr (read-from-file in)))
(define kills (kill-list instructions))
(define ins (in-list instructions))
(define outs (out-list instructions))