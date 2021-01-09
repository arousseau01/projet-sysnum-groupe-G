    .section text
    jal %rax %rbx
    jal %rax L0
    jmp %rax
L0:
    jmp L0