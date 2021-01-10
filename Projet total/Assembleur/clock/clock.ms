    .section text
    ; -9 secondes, -10 minutes, -11 heures, -12 jours, -13 mois, -14 années
    ; rax sert pour l'adressage mémoire (stocke un des nombres ci-dessus)
    ; rbx sert au décompte de la date (stocke le nombre de secondes, minutes...)
    ; rcx sert aux comparaisons (stocke le nombre max de secondes,minutes... ou le numéro du mois)
    ; A chaque étape correspond l'incrémentation de son nom, impair, pair et février incrémentent les jours

.secondes:
    mov %rax -9
    mov %rbx (%rax)
    add %rbx %rbx 1
    mov %rcx 60
    beq %rbx %rcx .minutes
    mov (%rax) %rbx

.minutes:
    mov (%rax) 0
    mov %rax -10
    mov %rbx (%rax)
    add %rbx %rbx 1
    mov %rcx 60
    beq %rbx %rcx .heures
    mov (%rax) %rbx

.heures:
    mov (%rax) 0
    mov %rax -11
    mov %rbx (%rax)
    add %rbx %rbx 1
    mov %rcx 24
    beq %rbx %rcx .jours
    mov (%rax) %rbx

.jours:
    mov (%rax) 0
    mov %rax -12
    mov %rbx (%rax)
    add %rbx %rbx 1

    mov %rax -13 
    mov %rcx 1
    beq %rax %rcx .impair
    mov %rcx 3
    beq %rax %rcx .impair
    mov %rcx 5
    beq %rax %rcx .impair
    mov %rcx 7
    beq %rax %rcx .impair
    mov %rcx 8
    beq %rax %rcx .impair
    mov %rcx 10
    beq %rax %rcx .impair
    mov %rcx 12
    beq %rax %rcx .impair
    mov %rcx 4
    beq %rax %rcx .pair
    mov %rcx 6
    beq %rax %rcx .pair
    mov %rcx 9
    beq %rax %rcx .pair
    mov %rcx 11
    beq %rax %rcx .pair
    mov %rcx 2
    beq %rax %rcx .février

.impair:
    mov %rcx 31
    beq %rbx %rcx .mois
    mov %rax -12
    mov (%rax) %rbx

.pair:
    mov %rcx 30
    beq %rbx %rcx .mois
    mov %rax -12
    mov (%rax) %rbx

.février:
    mov %rcx 28
    beq %rbx %rcx .mois
    mov %rax -12
    mov (%rax) %rbx

.mois:
    mov %rax -12
    mov (%rax) 1
    mov %rax -13
    mov %rbx (%rax)
    add %rbx %rbx 1
    mov %rcx 13
    beq %rbx %rcx .années
    mov (%rax) %rbx

.années:
    mov (%rax) 1
    mov %rax -14
    mov %rbx (%rax)
    add %rbx %rbx 1
    mov (%rax) %rbx