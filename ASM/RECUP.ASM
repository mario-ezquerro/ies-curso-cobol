buffer equ 0B800H

pila segment stack
   db 128 dup ('pila')
pila ends
cgroup group code
code segment byte public 'code'
   assume cs:code
codigo proc far
recup:
    nop
    push es
    push ax
    push cx
    push dx
    push di
    push si

    mov ax,buffer
    mov ds,ax
    mov si,4000
    mov es,ax
    mov di,0

    mov cx,4000
    cld
    rep movsb

    pop si
    pop di
    pop dx
    pop cx
    pop ax
    pop es
    xor ax,ax
    ret
codigo endp
code ends
    end recup
