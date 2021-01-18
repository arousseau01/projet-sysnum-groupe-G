    .section text
    mov %rax %rax
.main:
    mov %rax 0
    mov %rbx (%rax)
    mov %rcx 0
    beq %rbx %rcx .main
    print %rax; debug
    print %rbx; déclenche l'actualisation de l'horloge
    print %rcx; debug
    sub %rbx %rbx 1
    mov (%rax) %rbx