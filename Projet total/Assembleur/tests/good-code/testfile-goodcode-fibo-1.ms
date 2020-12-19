.section text
.main:
    mov %rax 0 
    mov %rbx 1
    mov %rcx 10
    mov %rfx 0
    bne %rcx %rfx .double    
    jmp .print

.fibo:
    mov %rdx %rbx
    add %rbx %rax %rbx
    mov %rax %rdx
    sub %rcx %rcx 1
    mov %rfx 0
    bne %rcx %rfx .double
    jmp .print

.print:
    print %rax
 
