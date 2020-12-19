.section text
.main:
    mov %rax 1 
    mov %rbx 1
    mov %rcx 10
    mov %rfx 0
    bne %rcx %rfx .double
    jmp .print

.double:
    add %rbx %rax %rbx
    mov %rax %rbx
    sub %rcx %rcx 1
    mov %rfx 0
    bne %rcx %rfx .double
    jmp .print

.print:
    print %rax