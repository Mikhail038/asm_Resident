.286
.model tiny
.code
org 100h

.radix 16
locals @@

Start:
                mov cx, 0FFFF
@@Cycle:
                inc ax
                cmp ax, 0
                jne @@Cycle

                inc bx
                inc dx

                loop @@Cycle

                ret
end Start
