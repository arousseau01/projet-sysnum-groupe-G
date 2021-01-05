    .section text
    ; initialise l'horloge avec les entrées
    ; -9/-1 secondes, -10/-2 minutes, -11/-3 heures, -12/-4 jours, -13/-5 mois, -14/-6 années
    ; rax sert pour l'adressage mémoire (stocke un des nombres ci-dessus)
    ; rbx sert au décompte de la date (stocke le nombre de secondes, minutes...)
    

.main:
    mov %rax -1
    mov %rbx (%rax)
    mov %rax -9
    mov (%rax) %rbx 

    mov %rax -2
    mov %rbx (%rax)
    mov %rax -10
    mov (%rax) %rbx

    mov %rax -3
    mov %rbx (%rax)
    mov %rax -11
    mov (%rax) %rbx

    mov %rax -4
    mov %rbx (%rax)
    mov %rax -12
    mov (%rax) %rbx

    mov %rax -5
    mov %rbx (%rax)
    mov %rax -13
    mov (%rax) %rbx

    mov %rax -6
    mov %rbx (%rax)
    mov %rax -14
    mov (%rax) %rbx
