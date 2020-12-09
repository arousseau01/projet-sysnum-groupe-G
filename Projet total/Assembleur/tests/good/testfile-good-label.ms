; labels are followed by a colon
    .section text
.L0:
3L1:
    add %rax %rbx %rcx
S_hi:

; they can be in text or data
    .section data
.L42:
54ha: