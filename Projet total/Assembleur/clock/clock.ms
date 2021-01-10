    .section text
    ; -9 secondes, -10 minutes, -11 heures, -12 jours, -13 mois, -14 années
    ; rax sert pour l'adressage mémoire (stocke un des nombres ci-dessus)
    ; rbx sert au décompte de la date (stocke le nombre de secondes, minutes...)
    ; rcx sert aux comparaisons (stocke le nombre max de secondes,minutes... ou le numéro des mois impairs/pairs/février)
    ; rdx sert à déterminer la longueur du mois (contient le numéro du mois)
    ; A chaque étape correspond l'incrémentation de son nom, impair, pair et février incrémentent les jours

.init:
    ; initialise l'horloge avec les entrées
    ; -9/-1 secondes, -10/-2 minutes, -11/-3 heures, -12/-4 jours, -13/-5 mois, -14/-6 années
    ; rax sert pour l'adressage mémoire (stocke un des nombres ci-dessus)
    ; rbx sert au décompte de la date (stocke le nombre de secondes, minutes...)
    mov %rax -1
    mov %rbx 0;(%rax)
    mov %rax -9
    mov (%rax) %rbx 

    mov %rax -2
    mov %rbx 0;(%rax)
    mov %rax -10
    mov (%rax) %rbx

    mov %rax -3
    mov %rbx 0;(%rax)
    mov %rax -11
    mov (%rax) %rbx

    mov %rax -4
    mov %rbx 1;(%rax)
    mov %rax -12
    mov (%rax) %rbx

    mov %rax -5
    mov %rbx 1;(%rax)
    mov %rax -13
    mov (%rax) %rbx

    mov %rax -6
    mov %rbx 2021;(%rax)
    mov %rax -14
    mov (%rax) %rbx

.main:
    print %rax; déclenche l'actualisation de l'horloge
.secondes:
    mov %rax -9
    mov %rbx (%rax)
    add %rbx %rbx 1
    mov %rcx 60
    beq %rbx %rcx .minutes
    mov (%rax) %rbx
    jmp .main

.minutes:
    mov %rbx 0
    mov (%rax) %rbx
    mov %rax -10
    mov %rbx (%rax)
    add %rbx %rbx 1
    mov %rcx 60
    beq %rbx %rcx .heures
    mov (%rax) %rbx
    jmp .main

.heures:
    mov %rbx 0
    mov (%rax) %rbx
    mov %rax -11
    mov %rbx (%rax)
    add %rbx %rbx 1
    mov %rcx 24
    beq %rbx %rcx .jours
    mov (%rax) %rbx
    jmp .main

.jours:
    mov %rbx 0
    mov (%rax) %rbx
    mov %rax -12
    mov %rbx (%rax)
    add %rbx %rbx 1
    mov %rax -13 
    mov %rdx (%rax)
    mov %rcx 4
    beq %rdx %rcx .pair
    mov %rcx 6
    beq %rdx %rcx .pair
    mov %rcx 9
    beq %rdx %rcx .pair
    mov %rcx 11
    beq %rdx %rcx .pair
    mov %rcx 2
    beq %rdx %rcx .fevrier

.impair:
    mov %rcx 31
    jmp .jours2
.pair:
    mov %rcx 30
    jmp .jours2
.fevrier:
    mov %rcx 28

.jours2:
    beq %rbx %rcx .mois
    mov %rax -12
    mov (%rax) %rbx
    jmp .main


.mois:
    mov %rax -12
    mov %rbx 1
    mov (%rax) %rbx
    mov %rax -13
    mov %rbx (%rax)
    add %rbx %rbx 1
    mov %rcx 13
    beq %rbx %rcx .ans
    mov (%rax) %rbx
    jmp .main

.ans:
    mov %rbx 1
    mov (%rax) %rbx
    mov %rax -14
    mov %rbx (%rax)
    add %rbx %rbx 1
    mov (%rax) %rbx
    jmp .main
