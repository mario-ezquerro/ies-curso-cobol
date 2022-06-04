Dosseg
.Model Small
.Stack 100h
.Code
;Pila Segment stack
;   db 128 dup ('pila')
;Pila ends
Cgroup group code
code segment byte public 'code'
      assume cs:cgroup
Codigo proc far
Curon:
;   nop
;   push es
;   push ax
;   push cx
;   push dx
;   push di
;   push si

   mov ah, 01h
   mov ch, 6
   mov cl, 7
   int 10h

; Rutina para volver al DOS
;   mov ah, 4ch
;   int 21h

;   pop si
;   pop di
;   pop dx
;   pop cx
;   pop ax
;   pop es
   xor ax,ax
   ret
Codigo endp
Code ends
     End Curon
