#lang racket
(require "L5.rkt")
(require "L4.rkt")
(require "L3.rkt")
(require "L2.rkt")
(require "L1.rkt")

(define in (call-with-input-file "filename.in" read))

(define (read-from-file filename)
  (call-with-input-file filename read))

(define (write-file x)
  (with-output-to-file "prog.S" #:exists 'append (lambda () (print-line x))))

(when (file-exists? "prog.S")
  (delete-file "prog.S"))

(compile-program 
 (compile-L2
  (compile-L3 
   (compile-L4 
    (compile-L5 (read-from-file in))))))
