.text
.globl go
go:
pushq %rbx
pushq %rbp
pushq %r12
pushq %r13
pushq %r14
pushq %r15
call _main
popq %r15
popq %r14
popq %r13
popq %r12
popq %rbp
popq %rbx
retq
_main:
subq $0, %rsp
movq $-11, %rdi
movq $_f_ret, -8(%rsp)
subq $8, %rsp
jmp _f
_f_ret:
movq %rax, %rdi
imulq $2, %rdi
addq $1, %rdi
call print
addq $0, %rsp
ret
_f:
subq $8, %rsp
cmpq $0, %rdi
jl _tt
jmp _ff
_tt:
movq %rdi, 0(%rsp)
movq 0(%rsp), %rax
salq $1, %rdi
subq %rdi, %rax
ret
_ff:
movq %rdi, %rax
ret
addq $8, %rsp
ret
_main:
subq $0, %rsp
ret
addq $0, %rsp
ret
_main:
subq $0, %rsp
ret
ret
addq $0, %rsp
ret
