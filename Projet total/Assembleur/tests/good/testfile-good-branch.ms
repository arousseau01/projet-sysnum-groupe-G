    .section text
    beq %rax %rbx L0
    bne %rax %rbx L0
L0:
    ble %rax %rbx L0
    blt %rax %rbx L0
    bge %rax %rbx L0
    bgt %rax %rbx L0