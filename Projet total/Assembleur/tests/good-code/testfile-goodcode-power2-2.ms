.section text
.main:
    mov %rax 1 
    mov %rbx 1
    mov %rcx 10
    jal %rfx .print
    mov %rfx 0
    bne %rcx %rfx .double

.double:
    add %rbx %rax %rbx
    mov %rax %rbx
    sub %rcx %rcx 1
    jal %rfx .print
    mov %rfx 0
    bne %rcx %rfx .double
    
.print:
    print %rax
    jump %rfx
 