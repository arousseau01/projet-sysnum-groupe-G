.section text
.main:
    mov %rax 1 
    mov %rbx 1
    mov %rcx 42
    bne %rcx 0 .double
    jmp .print

.double:
    add %rbx %rax %rbx
    mov %rax %rbx
    sub %rcx %rcx 1
    bne %rcx 0 .double
    jmp .print

.print:
    print %rax
    jump %rfx