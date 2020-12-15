.section text
.main:
    mov %rax 1 
    mov %rbx 1
    mov %rcx 42
    jal %rfx .print
    bne %rcx 0 .double

.double:
    add %rbx %rax %rbx
    mov %rax %rbx
    sub %rcx %rcx 1
    jal %rfx .print
    bne %rcx 0 .double

.print:
    print %rax
    jump %rfx
 