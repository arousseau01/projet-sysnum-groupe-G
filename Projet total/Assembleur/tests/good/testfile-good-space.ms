    .section data
x:
y:
    .space 4
34z:
    .space 2

    .section text
    add %rax %rbx 42
    .space 2
L0:
    sub %rax %rbx 42