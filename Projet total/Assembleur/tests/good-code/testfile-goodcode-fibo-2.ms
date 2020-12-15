.section text
.main:
    mov %rax 0 
    mov %rbx 1
    mov %rcx 42
    jal %rfx .print
    bne %rcx 0 .fibo

.fibo:
    mov %rdx %rbx
    add %rbx %rax %rbx
    mov %rax %rdx
    sub %rcx %rcx 1
    jal %rfx .print
    bne %rcx 0 .fibo

.print:
    print %rax
    jump %rfx
 