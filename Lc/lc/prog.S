.text
.globl go
go:
pushq %rbx
pushq %rbp
pushq %r12
pushq %r13
pushq %r14
pushq %r15
call _L3_main
popq %r15
popq %r14
popq %r13
popq %r12
popq %rbp
popq %rbx
retq
_L3_main:
subq $96, %rsp
movq $3, %rdi
movq $1, %rsi
call allocate
movq %rax, 88(%rsp)
movq 88(%rsp), %r10
movq $1, 8(%r10)
movq $1, %rdi
movq $1, %rsi
call allocate
movq %rax, 80(%rsp)
movq $5, %rdi
movq $1, %rsi
call allocate
movq %rax, 72(%rsp)
movq 72(%rsp), %r10
movq $_label_0, 8(%r10)
movq 72(%rsp), %r10
movq 80(%rsp), %r11
movq %r11, 16(%r10)
movq $1, 64(%rsp)
movq 64(%rsp), %r10
sarq $1, %r10
movq %r10, 64(%rsp)
movq 88(%rsp), %r10
movq 0(%r10), %r10
movq %r10, 24(%rsp)
movq 64(%rsp), %r10
cmpq $0, %r10
jl _lbl1
jmp _lbl2
_lbl2:
movq 24(%rsp), %r11
movq 64(%rsp), %r10
cmpq %r11, %r10
jl _lbl0
jmp _lbl1
_lbl1:
movq 88(%rsp), %rdi
movq $1, %rsi
call array_error
_lbl0:
movq 64(%rsp), %r10
imulq $8, %r10
movq %r10, 64(%rsp)
movq 64(%rsp), %r10
movq 88(%rsp), %r11
addq %r11, %r10
movq %r10, 64(%rsp)
movq 64(%rsp), %r10
movq 72(%rsp), %r11
movq %r11, 8(%r10)
movq $1, 64(%rsp)
movq $1, 56(%rsp)
movq 56(%rsp), %r10
sarq $1, %r10
movq %r10, 56(%rsp)
movq 88(%rsp), %r10
movq 0(%r10), %r10
movq %r10, 16(%rsp)
movq 56(%rsp), %r10
cmpq $0, %r10
jl _lbl4
jmp _lbl5
_lbl5:
movq 16(%rsp), %r11
movq 56(%rsp), %r10
cmpq %r11, %r10
jl _lbl3
jmp _lbl4
_lbl4:
movq 88(%rsp), %rdi
movq $1, %rsi
call array_error
_lbl3:
movq 56(%rsp), %r10
imulq $8, %r10
movq %r10, 56(%rsp)
movq 56(%rsp), %r10
movq 88(%rsp), %r11
addq %r11, %r10
movq %r10, 56(%rsp)
movq 56(%rsp), %r10
movq 8(%r10), %r10
movq %r10, 56(%rsp)
movq $1, 48(%rsp)
movq 48(%rsp), %r10
sarq $1, %r10
movq %r10, 48(%rsp)
movq 56(%rsp), %r10
movq 0(%r10), %r10
movq %r10, 8(%rsp)
movq 48(%rsp), %r10
cmpq $0, %r10
jl _lbl7
jmp _lbl8
_lbl8:
movq 8(%rsp), %r11
movq 48(%rsp), %r10
cmpq %r11, %r10
jl _lbl6
jmp _lbl7
_lbl7:
movq 56(%rsp), %rdi
movq $1, %rsi
call array_error
_lbl6:
movq 48(%rsp), %r10
imulq $8, %r10
movq %r10, 48(%rsp)
movq 48(%rsp), %r10
movq 56(%rsp), %r11
addq %r11, %r10
movq %r10, 48(%rsp)
movq 48(%rsp), %r10
movq 8(%r10), %r10
movq %r10, 48(%rsp)
movq $3, 40(%rsp)
movq 40(%rsp), %r10
sarq $1, %r10
movq %r10, 40(%rsp)
movq 56(%rsp), %r10
movq 0(%r10), %r10
movq %r10, 0(%rsp)
movq 40(%rsp), %r10
cmpq $0, %r10
jl _lbl10
jmp _lbl11
_lbl11:
movq 0(%rsp), %r10
movq 40(%rsp), %r11
cmpq %r10, %r11
jl _lbl9
jmp _lbl10
_lbl10:
movq 56(%rsp), %rdi
movq $3, %rsi
call array_error
_lbl9:
movq 40(%rsp), %r10
imulq $8, %r10
movq %r10, 40(%rsp)
movq 40(%rsp), %r10
movq 56(%rsp), %r11
addq %r11, %r10
movq %r10, 40(%rsp)
movq 40(%rsp), %r10
movq 8(%r10), %r10
movq %r10, 40(%rsp)
movq $_lbl12, -8(%rsp)
movq 40(%rsp), %rdi
movq $5, %rsi
movq 48(%rsp), %r10
subq $8, %rsp
jmp %r10
_lbl12:
movq %rax, 32(%rsp)
movq 32(%rsp), %rdi
call print
addq $96, %rsp
ret
_label_0:
subq $0, %rsp
movq %rdi, %r10
movq %rsi, %r10
movq $3, %rax
addq $0, %rsp
ret
