Dosseg
.Model Small
.Stack 100h
.Code
;pila segment stack
;   db 128 dup ('pila')
;pila ends
cgroup group code
code segment byte public 'code'
      ASSUME CS:CGROUP
Codigo proc far
Curoff:
;   nop
;   push es
;   push ax
;   push cx
;   push dx
;   push di
;   push si

   mov ah, 01h
   mov ch, 20h
   int 10h

; Rutina para regresar al DOS
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
codigo endp
code ends
     end curoff
