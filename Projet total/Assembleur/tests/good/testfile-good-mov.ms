    .section text
mov %rax %rbx
mov %rax 42
mov (%rax) %rbx
mov %rax (%rbx)
mov y %rax
mov %rax 7lab

    .section data
x:
y:
    .space 2
7lab:

