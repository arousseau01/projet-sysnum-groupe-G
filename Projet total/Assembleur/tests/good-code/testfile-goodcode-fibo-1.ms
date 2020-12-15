.section text
.main:
    mov %rax 0 
    mov %rbx 1
    mov %rcx 42
    bne %rcx 0 .fibo
    jmp .print

.fibo:
    mov %rdx %rbx
    add %rbx %rax %rbx
    mov %rax %rdx
    sub %rcx %rcx 1
    bne %rcx 0 .fibo
    jmp .print

.print:
    print %rax
 
