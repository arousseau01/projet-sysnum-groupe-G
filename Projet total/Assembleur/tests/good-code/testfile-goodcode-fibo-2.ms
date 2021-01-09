.section text
.main:
    mov %rax 0 
    mov %rbx 1
    mov %rcx 10
    jal %rfx .print
    mov %rfx 0
    bne %rcx %rfx .double

.fibo:
    mov %rdx %rbx
    add %rbx %rax %rbx
    mov %rax %rdx
    sub %rcx %rcx 1
    jal %rfx .print
    mov %rfx 0
    bne %rcx %rfx .double
    
.print:
    print %rax
    jump %rfx
 