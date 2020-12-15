    .section text
.main:
    mov %rax 42
    mov n %rax

    mov %rax 0 
    mov %rbx 1
    mov %rcx n

    mov %rfx 0
    bne %rcx %rfx .fibo
    jmp .print
    
.fibo:
    mov %rdx %rbx
    add %rbx %rax %rbx
    mov %rax %rdx
    sub %rcx %rcx 1

    mov %rfx 0
    bne %rcx %rfx .fibo
    jmp .print
    
.print:
    ; print %rax
    
    .section data
n:  .space 2